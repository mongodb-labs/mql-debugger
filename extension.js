const vscode = require("vscode");

const EXTNAME = "MQL-Debugger";

const logger = vscode.window.createOutputChannel(EXTNAME);

module.exports = {
  async activate(context) {
    try {
      const parser = await import("./parser.mjs");
      if (parser === null) {
        logger.appendLine("[DEBUG] Failed to load parser");
        return;
      }
      logger.appendLine("activating");
      context.subscriptions.push(
        vscode.commands.registerCommand(
          "extension.mqldebugger.debugCurrentEditor",
          async () => {
            const editor = vscode.window.activeTextEditor;
            if (!editor) {
              vscode.window.showErrorMessage(
                "[MQL-Debugger] can not get edditor object."
              );
              return;
            }

            const startFile = editor.document.fileName;
            const selection = editor.selection;
            const text = editor.document.lineAt(selection.start.line).text;
            const fullText = editor.document.getText();
            logger.appendLine("[DEBUG] startFile: " + startFile);
            logger.appendLine("[DEBUG] selected line: " + selection.start.line);
            logger.appendLine("[DEBUG] selected text: " + text);
            logger.appendLine("[DEBUG] full text: " + fullText);
            const posed = parser.parseWithPos(fullText);
            logger.appendLine(
              "[DEBUG] content: " + JSON.stringify(posed, undefined, 4)
            );
            context.workspaceState.update("posed", posed);
          }
        )
      );
    } catch (err) {
      logger.appendLine(`failed to activate extension: ${err}`);
    }
  },
  deactivate() {
    logger.appendLine("deactivated");
  },
};
