import type { PDFDocumentProxy } from 'pdfjs-dist';

let workerReady: Promise<void> | null = null;

/** Loads pdf.js and its worker once, lazily. Never runs during prerender. */
async function ensureWorker(): Promise<void> {
	if (!workerReady) {
		workerReady = (async () => {
			const pdfjs = await import('pdfjs-dist');
			const workerSrc = (await import('pdfjs-dist/build/pdf.worker.min.mjs?url')).default;
			pdfjs.GlobalWorkerOptions.workerSrc = workerSrc;
		})();
	}
	return workerReady;
}

export interface RenderedPdf {
	pages: number;
	document: PDFDocumentProxy;
}

/** Renders the first page of a PDF into the supplied canvas. */
export async function renderFirstPage(
	url: string,
	canvas: HTMLCanvasElement,
	scale = 1.5
): Promise<RenderedPdf> {
	await ensureWorker();
	const pdfjs = await import('pdfjs-dist');

	const document = await pdfjs.getDocument({ url }).promise;
	const page = await document.getPage(1);
	const viewport = page.getViewport({ scale });

	canvas.width = viewport.width;
	canvas.height = viewport.height;

	const context = canvas.getContext('2d');
	if (!context) throw new Error('2D canvas context unavailable');

	const render = page.render as unknown as (params: unknown) => { promise: Promise<void> };
	await render.call(page, { canvasContext: context, viewport, canvas }).promise;

	return { pages: document.numPages, document };
}
