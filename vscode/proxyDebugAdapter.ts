/* eslint-disable @typescript-eslint/no-explicit-any */

import vscode = require('vscode');
import stream = require('stream');
import * as parser from "../output/Parser/index.js";
import { OutputEvent, TerminatedEvent } from 'vscode-debugadapter';
import { PipelineRunnerSession } from './pipelineRunnerSession';
// import { DebugProtocol } from 'vscode-debugprotocol';

export class MqlDebugAdapterTrackerFactory implements vscode.DebugAdapterTrackerFactory {
	constructor(private outputChannel?: vscode.OutputChannel) { }

	createDebugAdapterTracker(session: vscode.DebugSession): vscode.ProviderResult<vscode.DebugAdapterTracker> {
		const logger = this.outputChannel!;
		let inlayHint: vscode.Disposable | undefined;
		logger.appendLine(`[DEBUG] session type: ${typeof (session)}`)
		return {
			onWillStartSession: () =>
				logger.appendLine(`[DEBUG] session ${session.id} will start with ${JSON.stringify(session.configuration)}\n`),
			onWillReceiveMessage: (message: any) => {
				logger.appendLine(`[DEBUG] client -> ${JSON.stringify(message)}\n`);
			},
			onDidSendMessage: (message: any) => {
				logger.appendLine(`[DEBUG] client  <- ${JSON.stringify(message)}\n`);
				if (message.event === "inline") {
					logger.appendLine(`[DEBUG] getting decoration info\n`);
					logger.appendLine(`Inline Deco: ${JSON.stringify(message.body)}`);
					inlayHint && inlayHint.dispose();
					inlayHint = vscode.languages.registerInlayHintsProvider({ language: "json" }, {
						provideInlayHints(model, range, token) {
							return message.body.map((step: { pos: { col: number, row: number }, val: string }) => {
								const tokLen = parser.firstTokLen(vscode.window.activeTextEditor?.document.lineAt(step.pos.row - 1).text.slice(step.pos.col));
								const hint = new vscode.InlayHint(new vscode.Position(
									step.pos.row - 1,
									step.pos.col + tokLen
								), `-> ${step.val}`);
								hint.paddingLeft = true;
								hint.paddingRight = true;
								return hint;
							});
						},
					});
				}
			},
			onError: (error: Error) => logger.appendLine(`[ERROR] ${error}\n`),
			onWillStopSession: () => {
				logger.appendLine(`[DEBUG] session ${session.id} will stop\n`);
				inlayHint && inlayHint.dispose();
			},
			onExit: (code: number | undefined, signal: string | undefined) =>
				logger.appendLine(`[DEBUG] debug adapter exited: (code: ${code}, signal: ${signal})\n`)
		};
	}

	dispose() { }
}

export class MqlDebugAdapterDescriptorFactory implements vscode.DebugAdapterDescriptorFactory {
	constructor(private outputChannel?: vscode.OutputChannel) { }

	public createDebugAdapterDescriptor(
		session: vscode.DebugSession,
		e: vscode.DebugAdapterExecutable | undefined
	): vscode.ProviderResult<vscode.DebugAdapterDescriptor> {
		this.outputChannel!.appendLine(`[DEBUG] Debug session: ${session.type}`)
		if (session.type === 'mql') {
			this.outputChannel!.appendLine(`[DEBUG] Debug Session Config: ${JSON.stringify(session.configuration)}`);
			const d = new ProxyDebugAdapter((session as any) as PipelineRunnerSession, this.outputChannel);
			return new vscode.DebugAdapterInlineImplementation(d);
		}
		return e;
	}

	public async dispose() {
		this.outputChannel!.appendLine('MqlDebugAdapterDescriptorFactory.dispose');
	}
}

const TWO_CRLF = '\r\n\r\n';

export class ProxyDebugAdapter implements vscode.DebugAdapter {
	private messageEmitter = new vscode.EventEmitter<vscode.DebugProtocolMessage>();
	private readable: stream.Readable | null = null;
	private writable: stream.Writable | null = null;
	private terminated = false;
	private outputChannel?: vscode.OutputChannel;
	private session: PipelineRunnerSession;

	constructor(session: PipelineRunnerSession, outputChannel?: vscode.OutputChannel) {
		this.onDidSendMessage = this.messageEmitter.event;
		this.outputChannel = outputChannel;
		this.session = session;
	}

	onDidSendMessage: vscode.Event<vscode.DebugProtocolMessage>;

	async handleMessage(message: vscode.DebugProtocolMessage): Promise<void> {
		await this.sendMessageToServer(message);
		this.outputChannel!.appendLine(`[DEBUG] sent message to server: ${JSON.stringify(message)}`)
	}

	protected sendMessageToClient(msg: vscode.DebugProtocolMessage) {
		this.messageEmitter.fire(msg);
		this.outputChannel!.appendLine(`[DEBUG] sent message to client: ${msg}`)
	}

	protected sendMessageToServer(message: vscode.DebugProtocolMessage): void {
		if (!this.writable) {
			const [stdin, stdout] = this.session.getStdIO();
			this.writable = stdin;
			this.readable = stdout;
		}
		const json = JSON.stringify(message) ?? '';
		if (this.writable) {
			this.writable.write(
				`Content-Length: ${Buffer.byteLength(json, 'utf8')}${TWO_CRLF}${json}`,
				'utf8',
				(err) => {
					if (err) {
						this.sendMessageToClient(new TerminatedEvent());
					}
				}
			);
		}
	}

	public async start(readable: stream.Readable, writable: stream.Writable) {
		if (this.readable || this.writable) {
			throw new Error('start was called more than once');
		}
		this.outputChannel!.appendLine("[DEBUG] Proxy Adapter Starting")
		this.readable = readable;
		this.writable = writable;
		this.readable.on('data', (data: Buffer) => {
			this.handleDataFromServer(data);
		});
		this.readable.once('close', () => {
			this.readable = null;
		});
		this.readable.on('error', (err) => {
			if (this.terminated) {
				return;
			}
			this.terminated = true;

			if (err) {
				this.sendMessageToClient(new OutputEvent(`connection error: ${err}\n`, 'console'));
			}
			this.sendMessageToClient(new TerminatedEvent());
		});
	}

	async dispose() {
		this.writable?.end(); // no more write.
	}

	private rawData = Buffer.alloc(0);
	private contentLength = -1;

	private handleDataFromServer(data: Buffer): void {
		this.rawData = Buffer.concat([this.rawData, data]);

		// eslint-disable-next-line no-constant-condition
		while (true) {
			if (this.contentLength >= 0) {
				if (this.rawData.length >= this.contentLength) {
					const message = this.rawData.toString('utf8', 0, this.contentLength);
					this.rawData = this.rawData.slice(this.contentLength);
					this.contentLength = -1;
					if (message.length > 0) {
						const rawMessage = JSON.parse(message);
						this.sendMessageToClient(rawMessage);
					}
					continue; // there may be more complete messages to process
				}
			} else {
				const idx = this.rawData.indexOf(TWO_CRLF);
				if (idx !== -1) {
					const header = this.rawData.toString('utf8', 0, idx);
					const lines = header.split('\r\n');
					for (const line of lines) {
						const pair = line.split(/: +/);
						if (pair[0] === 'Content-Length') {
							this.contentLength = +pair[1];
						}
					}
					this.rawData = this.rawData.slice(idx + TWO_CRLF.length);
					continue;
				}
			}
			break;
		}
	}
}
