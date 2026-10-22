import { sectionById, type LocationId, type SectionId } from '../content';
import type { WindowsApi } from './window-context';

/** Sections that live inside the finder rather than in a window of their own. */
const LOCATION_BY_SECTION: Partial<Record<SectionId, LocationId>> = {
	projects: 'work',
	archive: 'archive',
};

/**
 * Opens the window a section drives, navigating the finder for sections that live inside it.
 *
 * The archive shares the finder window, so it navigates instead of toggling — otherwise clicking it
 * while the finder is already open would just close it.
 */
export function openSection(windows: WindowsApi, id: SectionId): void {
	const meta = sectionById.get(id);
	if (!meta) return;

	const location = LOCATION_BY_SECTION[id];

	if (id === 'archive' && location) {
		windows.open('finder', { location });
		return;
	}

	windows.toggle(meta.windowId, location ? { location } : undefined);
}
