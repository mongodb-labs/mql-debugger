import { PipelineRunnerSession } from './pipelineRunnerSession';
// import { promises as fs } from 'fs';

// const fsAccessor = {
// 	isWindows: process.platform === 'win32',
// 	readFile(path: string): Promise<Uint8Array> {
// 		return fs.readFile(path);
// 	},
// 	writeFile(path: string, contents: Uint8Array): Promise<void> {
// 		return fs.writeFile(path, contents);
// 	}
// };

	const session = new PipelineRunnerSession();
	process.on('SIGTERM', () => {
		session.shutdown();
	});
	session.start(process.stdin, process.stdout);
