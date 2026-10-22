import { Dock } from './Dock';
import { Menubar } from './Menubar';
import { PhoneShell } from './PhoneShell';
import { ContextMenuHost } from './ContextMenuHost';
import { DesktopIcons } from './DesktopIcons';
import { Welcome } from './Welcome';
import { WindowManager } from './WindowManager';
import { desktop, desktopInfoFile, hasDoc, sections } from '../content';
import { contentMenu } from '../lib/content-menu';
import { useContextMenu } from '../lib/context-menu';
import { openSection } from '../lib/open-section';
import { useLongPress } from '../lib/long-press';
import { useWindows } from '../lib/window-context';

export function Desktop() {
	const windowsApi = useWindows();
	const { openAt } = useContextMenu();

	const backgroundMenu = contentMenu(desktop.contextMenu, (item) => {
		switch (item.action) {
			case 'open': {
				const section = item.section;
				return section ? { onSelect: () => openSection(windowsApi, section) } : undefined;
			}
			case 'info':
				return { onSelect: () => windowsApi.open('txtfile', { file: desktopInfoFile }) };
			default:
				return undefined;
		}
	});

	// Touch devices reach the desktop menu by long-pressing the wallpaper.
	const longPress = useLongPress((_, point) => openAt(point, backgroundMenu));

	/*
	 * The macOS desktop and the phone shell are separate layers: one wrapper hides the entire
	 * desktop below 640px, which also stops open windows escaping the mode switch (their `display`
	 * is set inline by <Window>, so a per-window media query cannot turn them off).
	 */
	// A docs site has no apps of its own, so the phone's home screen would be empty: keep the
	// desktop shell and let CSS turn the reader into a full-screen sheet instead.
	const docsOnly = hasDoc && sections.length === 0;

	return (
		<>
			<div class={`desktop-shell${docsOnly ? ' docs-only' : ''}`}>
				{/* Behind everything, so right-clicking empty wallpaper opens the desktop menu. */}
				<div
					class='desktop-surface'
					aria-hidden='true'
					{...longPress}
					onContextMenu={(event) => openAt(event, backgroundMenu)}
				/>
				<Menubar />
				<Welcome />
				<DesktopIcons />
				<WindowManager />
				<Dock />
			</div>

			<PhoneShell />

			{/* Outside both layers, so the menu works in either mode. */}
			<ContextMenuHost />
		</>
	);
}
