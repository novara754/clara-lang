import * as child_process from 'child_process';
import { TextDocument } from 'vscode-languageserver-textdocument';
import {
	CompletionItem,
	CompletionItemKind,
	createConnection,
	Diagnostic,
	DiagnosticSeverity,
	DidChangeConfigurationNotification,
	InitializeParams,
	InitializeResult,
	ProposedFeatures,
	TextDocuments,
	TextDocumentSyncKind,
} from 'vscode-languageserver/node';

const documents = new TextDocuments(TextDocument);

const connection = createConnection(ProposedFeatures.all);

let hasConfigurationCapability = false;
let hasWorkspaceFolderCapability = false;
// let hasDiagnosticRelatedInformationCapability = false;

connection.onInitialize((params: InitializeParams) => {
	const capabilities = params.capabilities;

	hasConfigurationCapability = Boolean(capabilities.workspace) && Boolean(capabilities.workspace.configuration);
	hasWorkspaceFolderCapability = Boolean(capabilities.workspace) && Boolean(capabilities.workspace.workspaceFolders);
	// hasDiagnosticRelatedInformationCapability =
	// 	Boolean(capabilities.textDocument) &&
	// 	Boolean(capabilities.textDocument.publishDiagnostics) &&
	// 	Boolean(capabilities.textDocument.publishDiagnostics.relatedInformation);

	const result: InitializeResult = {
		capabilities: {
			textDocumentSync: TextDocumentSyncKind.Incremental,
			completionProvider: {
				resolveProvider: true,
			},
		},
	};

	if (hasWorkspaceFolderCapability) {
		result.capabilities.workspace = {
			workspaceFolders: {
				supported: true,
			},
		};
	}

	return result;
});

connection.onInitialized(() => {
	if (hasConfigurationCapability) {
		void connection.client.register(DidChangeConfigurationNotification.type, undefined);
	}
	if (hasWorkspaceFolderCapability) {
		void connection.workspace.onDidChangeWorkspaceFolders(() => {
			connection.console.log('Workspace folder change event received.');
		});
	}
});

function runCompiler(sourceCode: string): Promise<string> {
	return new Promise((resolve, reject) => {
		try {
			const compiler = child_process.spawn('clara', ['--json-diagnostics', '--no-emit', '-'], { stdio: 'pipe' });

			compiler.stdin.setDefaultEncoding('utf-8');
			compiler.stdin.write(sourceCode);
			compiler.stdin.end();

			let stdout = '';
			compiler.stdout.on('data', (data) => {
				stdout += data;
			});

			compiler.stdout.on('close', () => {
				resolve(stdout);
			});
		} catch (e) {
			reject(e);
		}
	});
}

async function validateTextDocument(textDocument: TextDocument) {
	const text = textDocument.getText();

	try {
		const compilerOutput = await runCompiler(text);

		const diagnostics: Diagnostic[] = [];
		for (const line of compilerOutput.split('\n')) {
			if (line.length === 0) {
				continue;
			}

			interface CompilerDiagnostic {
				span: { start: number; len: number };
				message: string;
			}

			const diagnostic = JSON.parse(line) as CompilerDiagnostic;

			diagnostics.push({
				severity: DiagnosticSeverity.Error,
				range: {
					start: textDocument.positionAt(diagnostic.span.start),
					end: textDocument.positionAt(diagnostic.span.start + diagnostic.span.len),
				},
				message: diagnostic.message,
				source: 'clara-language-server',
			});
		}

		void connection.sendDiagnostics({ uri: textDocument.uri, diagnostics });
	} catch (e) {
		connection.console.error((e as Error).message);
	}
}

documents.onDidChangeContent((change) => {
	void validateTextDocument(change.document);
});

connection.onCompletion(() => [
	{
		label: 'TypeScript',
		kind: CompletionItemKind.Text,
		data: 1,
	},
	{
		label: 'JavaScript',
		kind: CompletionItemKind.Text,
		data: 2,
	},
]);

connection.onCompletionResolve((item: CompletionItem) => {
	if (item.data === 1) {
		item.detail = 'TypeScript details';
		item.documentation = 'Typescript documentation';
	} else if (item.data === 2) {
		item.detail = 'JavaScript details';
		item.documentation = 'JavaScript documentation';
	}
	return item;
});

documents.listen(connection);
connection.listen();
