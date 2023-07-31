import {
  Logger,
  logger,
  LoggingDebugSession,
  InitializedEvent,
  // TerminatedEvent,
  //  BreakpointEvent,
  // ProgressStartEvent,
  // ProgressUpdateEvent,
  // ProgressEndEvent,
  Thread,
  StackFrame,
  Scope,
  Source,
  OutputEvent,
  //  InvalidatedEvent,
  StoppedEvent,
  ContinuedEvent,
  Handles,
  Breakpoint,
  Event,
  // MemoryEvent,
} from "@vscode/debugadapter";
import { DebugProtocol } from "@vscode/debugprotocol";
import * as child from "child_process";
import * as parser from "../output/Parser/index.js";
import { promises as fs } from "fs";
import { assert } from "console";
import { Signal } from "typed-signals";
import { basename } from "path";
import * as stream from "stream";
import { waitForSignals } from 'wait-for-signals';
import { Collector } from "wait-for-signals/build/lib/Collector.js";

interface ILaunchRequestArguments extends DebugProtocol.LaunchRequestArguments {
  runnerPath: string;
  filePath: string;
}

interface ObservationStep {
  sourceLocId: number;
  typeTag: number;
  typeName: string;
  value: string;
};

export class InlineEvent extends Event {
  constructor(data: any) {
    super("inline", data);
  }
};

export class PipelineRunnerSession extends LoggingDebugSession {
  // private static threadID = 1;

  private _runner: child.ChildProcess | null = null;
  private _outputs: Signal<(output: string) => void> = new Signal<(output: string) => void>();
  private _processedPipeline: Object = {};
  private _observationSteps: Array<ObservationStep> = [];
  private _stepPos: number = 0;
  private _pipelineOutputs: Array<Object> = [];
  private _sourcePath: string = "";
  private _sourceLocIdMap: { [key: number]: { pos: { col: number, row: number, endCol: number, endRow: number }, name: string } } = {};
  private _varHandles = new Handles<'Pipeline' | 'Stage' | string>();
  private _breakpoints: Array<{ col: number, row: number }> = [];
  private _exprRanges: Array<{ startCol: number, startRow: number, endCol: number, endRow: number, srcLocId: number }> = [];
  private _evalOutputSignal: Collector | null = null;
  private _evalResult: string = "";

  public constructor() {
    super();
    this.setDebuggerLinesStartAt1(true);
    this.setDebuggerColumnsStartAt1(true);
    this._outputs.connect((d) => this.outputHandler(d));
  }

  public getStdIO(): [stream.Writable | null, stream.Readable | null] {
    if (this._runner) {
      return [this._runner.stdin, this._runner.stdout];
    }
    return [null, null]
  }

  public getSourcePath(): string {
    return this._sourcePath;
  }

  private _mapSourceLoc(merged: any) {
    if (Array.isArray(merged)) {
      merged.forEach((o) => this._mapSourceLoc(o));
    } else if (typeof (merged) === 'object') {
      const keys = Object.keys(merged);
      if (keys.includes("sourceLocId") && keys.includes("__srcPos")) {
        const name = keys.includes("originalExpr") ? merged["originalExpr"] : keys.filter((n) => { return n.startsWith("$") })[0];
        logger.log("[DEBUG] name for: " + JSON.stringify(merged) + " is " + name);
        logger.log("[DEBUG] keys: " + JSON.stringify(keys));
        const srcPos = merged["__srcPos"] as { col: number, row: number, endCol: number, endRow: number };
        this._sourceLocIdMap[merged["sourceLocId"]] = {
          pos: srcPos,
          name: name
        }
        this._exprRanges.push({
          srcLocId: merged["sourceLocId"],
          startCol: srcPos.col,
          startRow: srcPos.row,
          endCol: srcPos.endCol,
          endRow: srcPos.endRow
        })
      }
      keys.forEach((k) => {
        if (k !== '__srcPos' && k !== "sourceLocId") {
          this._mapSourceLoc(merged[k]);
        }
      })
    }
  }

  private async outputHandler(data: string) {
    logger.log("[DEBUG] runner out message: " + data);
    const lines = data.toString().trim().split("\n");
    for (const line of lines) {
      const sep = line.indexOf(":");
      const outType = line.slice(0, sep).toString();
      const outData = line.slice(sep + 1).toString();
      logger.log("[DEBUG] runner out type: " + outType);
      logger.log("[DEBUG] runner out body: " + JSON.stringify(outData));
      if (outType.includes("annotated pipeline")) {
        try {
          logger.log("[DEBUG] merging processed pipelines");
          const anno = parser.simpleParse(outData);
          logger.log("[DEBUG] anno object: " + JSON.stringify(anno, null, 4));
          logger.log("[DEBUG] processed pipe: " + JSON.stringify(this._processedPipeline, null, 4));
          const merged = parser.mergeAwithP(anno)(this._processedPipeline);
          logger.log("[DEBUG] merged pipeline: " + JSON.stringify(merged, null, 4));
          this._mapSourceLoc(merged);
          for (const k in this._sourceLocIdMap) {
            logger.log("[DEBUG] merged source loc: " + JSON.stringify(k) + " " + JSON.stringify(this._sourceLocIdMap[k]));
          }
          this.sendEvent(new StoppedEvent("entry", 1));
        } catch (e) {
          logger.error(`[ERROR] ${e}`);
        }
      } else if (outType.includes("output")) {
        const outObj = parser.simpleParse(outData);
        this._pipelineOutputs.push(outObj);
        logger.log("[DEBUG] read output doc: " + JSON.stringify(outObj, null, 4));
        this._evalResult = JSON.stringify(outObj, null, 4);
        await this._evalOutputSignal?.signal();
        // this.sendEvent(new OutputEvent(JSON.stringify(outObj, null, 4), "stdout"));
        this.sendEvent(new StoppedEvent("pause", 1));
        // this.sendEvent(new InvalidatedEvent(["stacks"]));
      } else if (outType.includes("observed")) {
        const obObj = JSON.parse(outData) as ObservationStep;
        this._observationSteps.push(obObj);
        logger.log("[DEBUG] observation: " + JSON.stringify(obObj, null, 4));
      }
    }
  }

  protected initializeRequest(response: DebugProtocol.InitializeResponse, args: DebugProtocol.InitializeRequestArguments): void {
    response.body = response.body || {};
    // make VS Code show a 'step back' button
    response.body.supportsStepBack = true;

    // make VS Code support data breakpoints
    response.body.supportsDataBreakpoints = true;
    // make VS Code send the breakpointLocations request
    response.body.supportsBreakpointLocationsRequest = true;

    // make VS Code provide "Step in Target" functionality
    response.body.supportsStepInTargetsRequest = true;
    response.body.supportsFunctionBreakpoints = true;
    response.body.supportsDelayedStackTraceLoading = false;
    this.sendResponse(response);

    // since this debug adapter can accept configuration requests like 'setBreakpoint' at any time,
    // we request them early by sending an 'initializeRequest' to the frontend.
    // The frontend will end the configuration sequence by calling 'configurationDone' request.
    this.sendEvent(new InitializedEvent());
  }

  protected async launchRequest(
    response: DebugProtocol.LaunchResponse,
    args: ILaunchRequestArguments
  ) {
    // make sure to 'Stop' the buffered logging if 'trace' is not set
    logger.setup(Logger.LogLevel.Warn, false);
    logger.log("[DEBUG] launching, filepath: " + args.filePath);
    this._sourcePath = args.filePath;
    const content = await fs.readFile(args.filePath, "utf8");
    logger.log("[DEBUG] content read: " + content);
    //    this._lines = content.split("\n");
    const posed = parser.parseWithPos(content);
    this._processedPipeline = posed;
    logger.log("[DEBUG] content: " + JSON.stringify(this._processedPipeline, null, 4));

    this._runner = child.spawn(
      args.runnerPath,
      [
        "--run_pipeline",
        "true",
        "--pipeline_filename",
        args.filePath,
        "--stdin",
        "true",
        "--debug",
        "true",
      ],
      { stdio: "pipe", cwd: "/" }
    );

    assert(this._runner != null);

    this._runner.stdout!.on("data", (data) => {
      this._outputs.emit(data);
    });

    this.sendEvent(new OutputEvent("MQL Debugger started", "stdout"));
    this.sendResponse(response);
  }

  private _sourceLocToExprObj(sourceLocId: number): { pos: { col: number, row: number }, name: string } {
    try {
      return this._sourceLocIdMap[sourceLocId];
    } catch (e) {
      return {
        pos: { row: 0, col: 0 },
        name: "unknown"
      };
    }
  }

  protected threadsRequest(response: DebugProtocol.ThreadsResponse): void {
    response.body = {
      threads: [
        new Thread(1, "thread 1"),
      ]
    };
    this.sendResponse(response);
  }

  protected setBreakPointsRequest(response: DebugProtocol.SetBreakpointsResponse, args: DebugProtocol.SetBreakpointsArguments, request?: DebugProtocol.Request | undefined): void {
    let breakpoints: Array<{ row: number, col: number }> = [];
    args.breakpoints?.forEach((brk) => {
      const { line: reqRow, column: reqCol } = brk;
      breakpoints.push(...this.identifyBreakPoints(reqRow, reqCol));
    })
    this._breakpoints = breakpoints;
    logger.log(`[DEBUG] Breakpoints set to ${JSON.stringify(this._breakpoints)}`);
    const srcObj = new Source(basename(this._sourcePath), this._sourcePath);
    response.body = {
      breakpoints: breakpoints.map((p) => {
        return new Breakpoint(true, p.row, p.col, srcObj);
      })
    };
    this.sendResponse(response)
  }

  private identifyBreakPoints(reqRow: number, reqCol?: number): Array<{ row: number, col: number }> {
    logger.log(`[DEBUG] finding possible breakpoint at row: ${reqRow} col: ${reqCol}`)
    let res: Array<{ row: number, col: number }> = [];
    if (!reqCol) {
      Object.values(this._sourceLocIdMap).forEach((entry) => {
        const p = entry.pos;
        if (p.row === reqRow) {
          res.push(p);
        }
      });
      if (res.length === 0 && reqRow > 1) {
        // A simple and stupid way to try to find the nearest possible breakpoint
        return this.identifyBreakPoints(reqRow - 1);
      }
    } else {
      Object.values(this._sourceLocIdMap).forEach((entry) => {
        const p = entry.pos;
        logger.log(`[DEBUG] Checking sourceLoc: ${JSON.stringify(p)}`)
        if (p.row === reqRow && p.col <= reqCol) {
          if (p.endCol >= reqCol) {
            logger.log(`[DEBUG] Matched`);
            res.push(p);
          }
        }
      });
    }
    return res;
  }

  protected breakpointLocationsRequest(response: DebugProtocol.BreakpointLocationsResponse, args: DebugProtocol.BreakpointLocationsArguments, request?: DebugProtocol.Request | undefined): void {
    response.body = {
      breakpoints: Object.values(this._exprRanges).map((entry) => {
        return {
          line: entry.startRow,
          column: entry.startCol,
          endColumn: entry.endCol,
          endLine: entry.endRow
        };
      })
    };
    this.sendResponse(response);
  }

  protected stackTraceRequest(response: DebugProtocol.StackTraceResponse, args: DebugProtocol.StackTraceArguments): void {
    const stk = this._observationSteps.slice(0, this._stepPos);

    const srcObj = new Source(basename(this._sourcePath), this._sourcePath);

    let inlines: { pos: { col: number, row: number }, val: string }[] = [];

    response.body = {
      stackFrames: stk.reverse().map((step, ix) => {
        const exprObj = this._sourceLocToExprObj(step["sourceLocId"]);
        logger.log("[DEBUG] reading observation step: " + JSON.stringify(step));
        logger.log("[DEBUG] reading exprObj: " + JSON.stringify(exprObj));
        const sf: DebugProtocol.StackFrame = new StackFrame(ix, exprObj.name, srcObj, exprObj.pos.row, exprObj.pos.col);
        sf.presentationHint = "label";
        inlines.push(
          {
            pos: exprObj.pos,
            val: step.value
          }
        );
        return sf;
      }),
      // 4 options for 'totalFrames':
      //omit totalFrames property: 	// VS Code has to probe/guess. Should result in a max. of two requests
      totalFrames: stk.length			// stk.count is the correct size, should result in a max. of two requests
      //totalFrames: 1000000 			// not the correct size, should result in a max. of two requests
      //totalFrames: endFrame + 20 	// dynamically increases the size with every requested chunk, results in paging
    };
    // this.sendEvent(new OutputEvent("", "inline", inlines));
    this.sendEvent(new InlineEvent(inlines));
    this.sendResponse(response);
  }

  private stepUntilBreakpoint(threadID: number): boolean {
    if (this._observationSteps.length < 1) {
      this.sendEvent(new StoppedEvent("pause", threadID));
      return false;
    } else {
      while (this._stepPos < this._observationSteps.length) {
        logger.log(`[DEBUG] current step: ${JSON.stringify(this._observationSteps[this._stepPos])}`);
        if (this._breakpoints.find((bp) => {
          const exprObj = this._sourceLocToExprObj(this._observationSteps[this._stepPos]["sourceLocId"]);
          logger.log(`[DEBUG] checking exprObj: ${JSON.stringify(exprObj)}`)
          return exprObj.pos.row === bp.row;
        }) !== undefined) {
          this.sendEvent(new StoppedEvent("breakpoint", threadID));
          logger.log("[DEBUG] stopped on breakpoint pos: " + this._stepPos);
          this._stepPos += 1;
          return false;
        }
        this._stepPos += 1;
        logger.log(`[DEBUG] increased step counter to ${this._stepPos}`)
      }
    }
    return true;
  }

  protected nextRequest(response: DebugProtocol.NextResponse, args: DebugProtocol.NextArguments): void {
    if (this._stepPos < this._observationSteps.length) {
      this._stepPos += 1;
      logger.log("[DEBUG] advanced step pos to: " + this._stepPos);
    }
    this.sendResponse(response);
    this.sendEvent(new StoppedEvent("step", args.threadId));
  }

  protected stepBackRequest(response: DebugProtocol.StepBackResponse, args: DebugProtocol.StepBackArguments): void {
    if (this._stepPos > 0) {
      this._stepPos -= 1;
      logger.log("[DEBUG] rewinded step pos to: " + this._stepPos);
    }
    this.sendResponse(response);
    this.sendEvent(new StoppedEvent("step", args.threadId));
  }

  protected scopesRequest(response: DebugProtocol.ScopesResponse, args: DebugProtocol.ScopesArguments, request?: DebugProtocol.Request | undefined): void {
    response.body = {
      scopes: [
        new Scope("Pipeline", this._varHandles.create("Pipeline"), false),
        new Scope("Stage", this._varHandles.create("Stage"), false)
      ]
    };
    this.sendResponse(response);
  }

  protected variablesRequest(response: DebugProtocol.VariablesResponse, args: DebugProtocol.VariablesArguments, request?: DebugProtocol.Request | undefined): void {
    let vars: Map<number, DebugProtocol.Variable> = new Map();

    logger.log("[DEBUG] variable reference: " + args.variablesReference);

    this._observationSteps.slice(0, this._stepPos).forEach((step) => {
      const exprObj = this._sourceLocToExprObj(step["sourceLocId"]);
      let stepVar: DebugProtocol.Variable = {
        name: exprObj.name,
        value: step.value,
        type: step.typeName,
        variablesReference: args.variablesReference,
      };
      vars.set(step.sourceLocId, stepVar);
    });

    response.body = {
      variables: [...vars.entries()].sort((a, b) => a[0] - b[0]).map((x) => x[1])
    };

    this.sendResponse(response);
  }

  protected continueRequest(response: DebugProtocol.ContinueResponse, args: DebugProtocol.ContinueArguments, request?: DebugProtocol.Request | undefined): void {
    this.sendResponse(response);
    response.body = { allThreadsContinued: this.stepUntilBreakpoint(args.threadId) };
  }

  protected async evaluateRequest(
    response: DebugProtocol.EvaluateResponse,
    args: DebugProtocol.EvaluateArguments
  ): Promise<void> {
    response.type = "event";
    if (this._runner && args.context === "repl") {
      this._runner.stdin!.cork();
      this._runner.stdin!.write(args.expression.trim());
      this._runner.stdin!.write("\r\n");
      this._runner.stdin!.uncork();
      this._evalOutputSignal = waitForSignals({ count: 1 });
      await this._evalOutputSignal.promise;
      response.body = {
        result: this._evalResult,
        variablesReference: 0,
      };
    } else {
      response.body = {
        result: JSON.stringify({ expression: args.expression, context: args.context }),
        variablesReference: 0,
      };
    }
    this._stepPos = 0;
    this.sendResponse(response);
    this.sendEvent(new ContinuedEvent(1));
  }
}