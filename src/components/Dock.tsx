import { useEffect, useRef } from 'preact/hooks';

import {
	desktop,
	docSections,
	dockSections,
	icon,
	sectionById,
	type SectionId,
	type SectionMeta,
} from '../content';
import { contentMenu } from '../lib/content-menu';
import { useContextMenu } from '../lib/context-menu';
import { loadGsap } from '../lib/gsap';
import { useLongPress } from '../lib/long-press';
import { openSection } from '../lib/open-section';
import { useWindows } from '../lib/window-context';

/** macOS dock magnification: how much the hovered icon grows, and the falloff. */
const MAX_BOOST = 0.25;
const SIGMA = 48;
const MAGNIFY_EASE = { duration: 0.28, ease: 'power2.out', overwrite: 'auto' } as const;

/** Colourful dock artwork per doc section; the sidebar keeps the small line icons. */
const DOC_APP_ICONS: Record<string, string> = {
	overview: 'apps.finder',
	gompiler: 'apps.gompiler',
	go2cpp: 'apps.go2cpp',
	downloads: 'apps.downloads',
	usage: 'apps.usage',
};

export function Dock() {
	const windowsApi = useWindows();
	const { windows, close } = windowsApi;
	const { openAt } = useContextMenu();
	const containerRef = useRef<HTMLDivElement>(null);

	const menuFor = (section: SectionMeta) =>
		contentMenu(desktop.dockContextMenu, (item) => {
			switch (item.action) {
				case 'open':
					return { onSelect: () => openSection(windowsApi, section.id) };
				case 'quit':
					return {
						disabled: !windows[section.windowId].isOpen,
						onSelect: () => close(section.windowId),
					};
				default:
					return undefined;
			}
		});

	// Touch devices have no hover, so they reach the same menu by long-pressing an icon.
	const longPress = useLongPress((target, point) => {
		const id = target.dataset.longPress as SectionId | undefined;
		const section = id ? sectionById.get(id) : undefined;
		if (section) openAt(point, menuFor(section));
	});

	// Magnify icons by the pointer's distance, gliding via GSAP.
	useEffect(() => {
		if (window.matchMedia('(hover: none)').matches) return;

		const container = containerRef.current;
		if (!container) return;

		let cancelled = false;
		let detach: (() => void) | null = null;

		loadGsap()
			.then((gsap) => {
				if (cancelled) return;
				const icons = Array.from(container.querySelectorAll<HTMLElement>('.dock-icon'));
				if (!icons.length) return;

				// Measured at rest: the per-icon <ul>s are positioned, so `offsetLeft`
				// would report every icon at the same coordinate.
				const containerLeft = () => container.getBoundingClientRect().left;
				const centres = icons.map((icon) => {
					const rect = icon.getBoundingClientRect();
					return rect.left - containerLeft() + rect.width / 2;
				});

				const scaleTo = (icon: HTMLElement, scale: number) =>
					gsap.to(icon, { scale, ...MAGNIFY_EASE });

				const onMove = (event: MouseEvent) => {
					const x = event.clientX - containerLeft();
					icons.forEach((icon, index) => {
						const distance = (x - centres[index]) / SIGMA;
						scaleTo(icon, 1 + MAX_BOOST * Math.exp(-distance * distance));
					});
				};
				const onLeave = () => icons.forEach((icon) => scaleTo(icon, 1));

				// `mousemove` rather than `pointermove`: mouse input always emits it,
				// including synthetic CDP moves.
				container.addEventListener('mousemove', onMove);
				container.addEventListener('mouseleave', onLeave);
				detach = () => {
					container.removeEventListener('mousemove', onMove);
					container.removeEventListener('mouseleave', onLeave);
				};
			})
			.catch(() => undefined);

		return () => {
			cancelled = true;
			detach?.();
		};
	}, []);

	/**
	 * A docs site has no apps of its own, so the dock mirrors its sections — otherwise the
	 * desktop has an empty shelf where the portfolio has a dock.
	 */
	const docItems =
		dockSections.length === 0
			? docSections.map((section) => ({
					id: section.id,
					name: section.title,
					icon: icon(DOC_APP_ICONS[section.id] ?? 'files.folder'),
				}))
			: [];

	if (dockSections.length === 0 && docItems.length === 0) return null;

	const openDoc = (id: string) => {
		windowsApi.open('docs');
		window.location.hash = `#/${id}`;
	};

	return (
		<section id='dock' aria-label='Dock'>
			<div class='dock-container' ref={containerRef} {...longPress}>
				{dockSections.length > 0
					? dockSections.map((section) => (
							<ul key={section.id} class='group relative flex justify-center'>
								<li>
									<button
										type='button'
										class='dock-icon'
										aria-label={section.name}
										data-long-press={section.id}
										onClick={() => openSection(windowsApi, section.id)}
										onContextMenu={(event) => openAt(event, menuFor(section))}>
										<img src={section.icon} alt='' loading='lazy' />
									</button>
									<span
										role='tooltip'
										class='pointer-events-none absolute -top-9 left-1/2 -translate-x-1/2 whitespace-nowrap rounded-md bg-blue-200 px-3 py-1 text-xs text-blue-900 opacity-0 shadow-xl transition-opacity group-hover:opacity-100 group-focus-visible:opacity-100'>
										{section.name}
									</span>
								</li>
							</ul>
						))
					: docItems.map((item) => (
							<ul key={item.id} class='group relative flex justify-center'>
								<li>
									<button
										type='button'
										class='dock-icon'
										aria-label={item.name}
										onClick={() => openDoc(item.id)}>
										<img src={item.icon} alt='' loading='lazy' />
									</button>
									<span
										role='tooltip'
										class='pointer-events-none absolute -top-9 left-1/2 -translate-x-1/2 whitespace-nowrap rounded-md bg-blue-200 px-3 py-1 text-xs text-blue-900 opacity-0 shadow-xl transition-opacity group-hover:opacity-100 group-focus-visible:opacity-100'>
										{item.name}
									</span>
								</li>
							</ul>
						))}
			</div>
		</section>
	);
}
