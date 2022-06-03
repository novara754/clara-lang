import * as path from 'path';
import { ExtensionContext, workspace, window } from 'vscode';
import { ServerOptions, TransportKind, LanguageClientOptions, LanguageClient } from 'vscode-languageclient/node';

let client: LanguageClient | null = null;

export function activate(context: ExtensionContext) {
	const serverModule = context.asAbsolutePath(path.join('server', 'dist', 'src', 'main.js'));
	const debugOptions = { execArgv: ['--nolazy', '--inspect=6009'] };
	const serverOptions: ServerOptions = {
		run: {
			module: serverModule,
			transport: TransportKind.ipc,
		},
		debug: {
			module: serverModule,
			transport: TransportKind.ipc,
			options: debugOptions,
		},
	};

	const clientOptions: LanguageClientOptions = {
		documentSelector: [{ scheme: 'file', language: 'clara' }],
		synchronize: {
			fileEvents: workspace.createFileSystemWatcher('**/.clientrc'),
		},
	};
	client = new LanguageClient('clara-language-server', 'Clara LSP Client', serverOptions, clientOptions);

	void client.start().then(() => window.showInformationMessage('Clara LSP Client up and running!'));
}

export function deactivate(): Thenable<void> {
	if (!client) {
		return Promise.resolve();
	}
	return client.stop();
}
