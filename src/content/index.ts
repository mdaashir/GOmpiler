/**
 * The single import surface for everything under `content/`.
 *
 * Components import from here — never from a JSON file directly — so the skip rules live in one place.
 */
import { buildLocations, buildShortcuts } from './derive';
import { icon } from './icons';
import { desktop } from './loader';
import type { FileNode } from './types';

export * from './types';

export {
	desktop,
	dockSections,
	getSection,
	hasSection,
	hasWindowSection,
	interpolate,
	isSectionEnabled,
	menu,
	phone,
	profile,
	SECTION_IDS,
	sectionById,
	sectionData,
	sections,
	site,
	unknownSectionFiles,
} from './loader';

export type { SectionMeta } from './loader';

export { icon, icons, iconRoles } from './icons';

export * from './doc';

export { LOCATION_ORDER } from './derive';

/** The Finder filesystem, built from whichever sections have content. */
export const locations = buildLocations();

/** Desktop shortcuts, built from the projects section. */
export const shortcuts = buildShortcuts();

/** The file shown by "Get Info" on the desktop. */
export const desktopInfoFile: FileNode = {
	id: 'desktop-info',
	name: desktop.info.name,
	icon: icon('files.txt'),
	kind: 'file',
	fileType: 'txt',
	subtitle: desktop.info.subtitle,
	description: desktop.info.lines,
};
