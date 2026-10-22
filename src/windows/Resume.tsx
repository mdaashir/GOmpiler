import { useEffect, useRef, useState } from 'preact/hooks';

import { sectionData } from '../content';
import { renderFirstPage } from '../lib/pdf';

type Status = 'loading' | 'ready' | 'error';

export function Resume() {
	const section = sectionData.resume;
	const canvasRef = useRef<HTMLCanvasElement>(null);
	const [status, setStatus] = useState<Status>('loading');

	useEffect(() => {
		const canvas = canvasRef.current;
		if (!canvas || !section?.file) return;

		let cancelled = false;
		renderFirstPage(section.file, canvas)
			.then(() => {
				if (!cancelled) setStatus('ready');
			})
			.catch(() => {
				if (!cancelled) setStatus('error');
			});

		return () => {
			cancelled = true;
		};
	}, [section?.file]);

	if (!section) return null;

	return (
		<div class='resume__body'>
			{status === 'error' ? (
				<div class='p-8 text-center text-sm text-white/80'>
					{section.error && <p>{section.error}</p>}
					{section.openLabel && (
						<a
							class='mt-3 inline-block underline'
							href={section.file}
							target='_blank'
							rel='noopener noreferrer'>
							{section.openLabel}
						</a>
					)}
				</div>
			) : (
				<>
					{status === 'loading' && section.loading && (
						<p class='p-6 text-center text-sm text-white/70'>{section.loading}</p>
					)}
					<canvas ref={canvasRef} class='resume__canvas' />
				</>
			)}
		</div>
	);
}
