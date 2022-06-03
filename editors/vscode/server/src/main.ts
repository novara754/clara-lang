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
let hasDiagnosticRelatedInformationCapability = false;

connection.onInitialize((params: InitializeParams) => {
	const capabilities = params.capabilities;

	hasConfigurationCapability = Boolean(capabilities.workspace) && Boolean(capabilities.workspace.configuration);
	hasWorkspaceFolderCapability = Boolean(capabilities.workspace) && Boolean(capabilities.workspace.workspaceFolders);
	hasDiagnosticRelatedInformationCapability =
		Boolean(capabilities.textDocument) &&
		Boolean(capabilities.textDocument.publishDiagnostics) &&
		Boolean(capabilities.textDocument.publishDiagnostics.relatedInformation);

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

function validateTextDocument(textDocument: TextDocument) {
	const maxNumberOfProblems = 10;

	const text = textDocument.getText();
	const pattern = /\b[A-Z]{2,}\b/g;

	let m: RegExpExecArray | null;
	let problems = 0;
	const diagnostics: Diagnostic[] = [];
	while ((m = pattern.exec(text)) && problems < maxNumberOfProblems) {
		problems++;
		const diagnostic: Diagnostic = {
			severity: DiagnosticSeverity.Warning,
			range: {
				start: textDocument.positionAt(m.index),
				end: textDocument.positionAt(m.index + m[0].length),
			},
			message: `${m[0]!} is all uppercase.`,
			source: 'ex',
		};

		if (hasDiagnosticRelatedInformationCapability) {
			diagnostic.relatedInformation = [
				{
					location: {
						uri: textDocument.uri,
						range: Object.assign({}, diagnostic.range),
					},
					message: 'Spelling matters',
				},
				{
					location: {
						uri: textDocument.uri,
						range: Object.assign({}, diagnostic.range),
					},
					message: 'Particularly for names',
				},
			];
		}

		diagnostics.push(diagnostic);
	}

	diagnostics.push({
		severity: DiagnosticSeverity.Error,
		range: { start: textDocument.positionAt(1), end: textDocument.positionAt(2) },
		message: 'fuick you',
		source: 'ex',
	});

	void connection.sendDiagnostics({ uri: textDocument.uri, diagnostics });
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
