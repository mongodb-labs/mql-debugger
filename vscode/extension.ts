import * as vscode from "vscode";
import * as fs from "fs";
import * as path from "path";
import {
  // MqlDebugAdapterDescriptorFactory,
  MqlDebugAdapterTrackerFactory,
} from "./proxyDebugAdapter";

const EXTNAME = "MQL-Debugger";
const logger = vscode.window.createOutputChannel(EXTNAME);

export const activate = (context: vscode.ExtensionContext) => {
  try {
    context.subscriptions.push(logger);

    logger.appendLine("activating");

    if (vscode.workspace.workspaceFolders && 1 === vscode.workspace.workspaceFolders.length) {
      const wd = vscode.workspace.workspaceFolders[0].uri.fsPath;
      logger.appendLine("[INFO] preparing tasks.json file in " + wd);

      const vd = path.join(wd, ".vscode");
      if (!fs.existsSync(vd)) {
        logger.appendLine("[INFO] create .vscode folder. <" + vd + ">");
        fs.mkdirSync(vd);
      } else {
        logger.appendLine("[INFO] .vscode folder exists.");
      }

      const ep = path.join(vd, "tasks.json");
      if (!fs.existsSync(ep)) {
        logger.appendLine("[INFO] create tasks.json file. <" + ep + ">");
        fs.writeFileSync(ep, tasks_json);
      } else {
        logger.appendLine("[INFO] tasks.json file exists.");
      }
    }
    //const factory = new MqlDebugAdapterDescriptorFactory(logger);
    //context.subscriptions.push(vscode.debug.registerDebugAdapterDescriptorFactory("mql", factory));
    //if ("dispose" in factory) {
    //  context.subscriptions.push(factory);
    //}

    const tracker = new MqlDebugAdapterTrackerFactory(logger);
    context.subscriptions.push(vscode.debug.registerDebugAdapterTrackerFactory("mql", tracker));
    if ("dispose" in tracker) {
      context.subscriptions.push(tracker);
    }
    context.subscriptions.push(
      vscode.commands.registerCommand(
        "extension.mqldebugger.debugCurrentEditor",
        async (fileUri: vscode.Uri) => {
          const editor = vscode.window.activeTextEditor;
          if (!editor) {
            vscode.window.showErrorMessage("[MQL-Debugger] can not get edditor object.");
            return;
          }
          if (!vscode.workspace.getConfiguration("mqldebugger").has("pipelineRunnerPath")) {
            vscode.window.showErrorMessage("[CRITICAL] can not get pipeline runner path. ");
            return;
          }
          const pipelineRunnerPath = vscode.workspace
            .getConfiguration("mqldebugger")
            .get("pipelineRunnerPath");
          logger.appendLine("[DEBUG] pipelineRunnerPath: " + pipelineRunnerPath);

          const startFile = editor.document.fileName;
          const selection = editor.selection;
          const text = editor.document.lineAt(selection.start.line).text;
          const fullText = editor.document.getText();
          logger.appendLine("[DEBUG] startFile: " + startFile);
          logger.appendLine("[DEBUG] selected line: " + selection.start.line);
          logger.appendLine("[DEBUG] selected text: " + text);
          logger.appendLine("[DEBUG] full text: " + fullText);

          if (!fileUri && vscode.window.activeTextEditor) {
            fileUri = vscode.window.activeTextEditor.document.uri;
          }
          startSession(fileUri, pipelineRunnerPath as String);
        }
      )
    );
    context.subscriptions.push(
      vscode.commands.registerCommand("extension.mqldebugger.nextStep", async () => { })
    );
    context.subscriptions.push(
      vscode.commands.registerCommand("extension.mqldebugger.prevStep", async () => { })
    );
  } catch (err) {
    logger.appendLine(`failed to activate extension: ${err}`);
  }
};

export const deactivate = () => {
  logger.appendLine("deactivated");
};

const startSession = async (fileUri: vscode.Uri, runnerPath: String) => {
  vscode.debug.startDebugging(undefined, {
    type: "mql",
    name: "Debug File",
    request: "launch",
    filePath: fileUri.fsPath,
    runnerPath: runnerPath,
  });
};

const tasks_json: string = `
{
"version": "2.0.0",
"presentation": {
"reveal": "always",
"panel": "new"
},
"tasks": [
]
}
`;
