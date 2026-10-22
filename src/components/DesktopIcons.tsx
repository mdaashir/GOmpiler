import { useEffect, useRef } from 'preact/hooks';

import { desktop, desktopInfoFile, shortcuts, type DesktopShortcut } from '../content';
import { contentMenu } from '../lib/content-menu';
import { useContextMenu } from '../lib/context-menu';
import { loadDraggable } from '../lib/gsap';
import { useLongPress } from '../lib/long-press';
import { openSection } from '../lib/open-section';
import { useWindows } from '../lib/window-context';

export function DesktopIcons() {
	const windowsApi = useWindows();
	const { openAt } = useContextMenu();
	const rootRef = useRef<HTMLElement>(null);
	const draggedRef = useRef(false);

	// Desktop icons are freely draggable around the desktop.
	useEffect(() => {
		const root = rootRef.current;
		if (!root) return;

		let cancelled = false;
		let instances: { kill: () => void }[] = [];

		loadDraggable()
			.then((Draggable) => {
				if (cancelled || !root) return;
				instances = Array.from(root.querySelectorAll<HTMLElement>('.desktop-icon')).map(
					(element) =>
						Draggable.create(element, {
							type: 'x,y',
							bounds: root,
							onDragStart: () => {
								draggedRef.current = true;
							},
							onDragEnd: () => {
								window.setTimeout(() => {
									draggedRef.current = false;
								}, 120);
							},
						})[0]
				);
			})
			.catch(() => undefined);

		return () => {
			cancelled = true;
			instances.forEach((instance) => instance.kill());
		};
	}, [shortcuts.length]);

	const menuFor = (shortcut: DesktopShortcut) =>
		contentMenu(desktop.iconContextMenu, (item) => {
			switch (item.action) {
				case 'open':
					return { onSelect: () => openSection(windowsApi, 'projects') };
				case 'info':
					return {
						onSelect: () =>
							windowsApi.open('txtfile', {
								file: {
									...desktopInfoFile,
									name: `${shortcut.name}.txt`,
									subtitle: shortcut.name,
									description: shortcut.description ?? desktopInfoFile.description,
								},
							}),
					};
				default:
					return undefined;
			}
		});

	// Touch devices reach the same menu by long-pressing an icon.
	const longPress = useLongPress((target, point) => {
		const shortcut = shortcuts.find((entry) => entry.id === target.dataset.longPress);
		if (shortcut) openAt(point, menuFor(shortcut));
	});

	return (
		<section id='desktop-icons' aria-label='Desktop' ref={rootRef} {...longPress}>
			{shortcuts.map((shortcut) => (
				<button
					key={shortcut.id}
					type='button'
					class={`desktop-icon ${shortcut.position}`}
					data-long-press={shortcut.id}
					onClick={() => {
						if (draggedRef.current) return;
						openSection(windowsApi, 'projects');
					}}
					onContextMenu={(event) => openAt(event, menuFor(shortcut))}>
					<img src={shortcut.icon} alt='' />
					<span>{shortcut.name}</span>
				</button>
			))}
		</section>
	);
}
