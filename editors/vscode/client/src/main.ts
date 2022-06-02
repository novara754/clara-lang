import { ExtensionContext, window } from 'vscode';

export function activate(context: ExtensionContext) {
	window.showInformationMessage('hello world from clara language server!');
}

export function deactivate(context: ExtensionContext): Thenable<void> | undefined {
	return undefined;
}
