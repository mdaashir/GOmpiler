import desktopFile from '../../content/desktop.json';
import menuFile from '../../content/menu.json';
import phoneFile from '../../content/phone.json';
import profileFile from '../../content/profile.json';
import siteFile from '../../content/site.json';

import type { WindowId } from '../lib/windows';
import { resolveIcons } from './icons';
import type {
	DesktopConfig,
	MenuConfig,
	PhoneConfig,
	ProfileConfig,
	SectionData,
	SectionId,
	SiteConfig,
} from './types';

/* ---------- Shell files ---------- */

export const site = resolveIcons(siteFile as SiteConfig);
export const profile = resolveIcons(profileFile as ProfileConfig);
export const menu = resolveIcons(menuFile as MenuConfig);
export const desktop = resolveIcons(desktopFile as DesktopConfig);
export const phone = resolveIcons(phoneFile as PhoneConfig);

/** Replaces `{token}` with a value, leaving unknown tokens untouched. */
export function interpolate(text: string, values: Record<string, string | number>): string {
	return text.replace(/\{(\w+)\}/g, (match, key: string) =>
		key in values ? String(values[key]) : match
	);
}

/* ---------- Sections ---------- */

/** Which window each section drives — `archive` reuses the Finder. */
export const SECTION_WINDOWS: Record<SectionId, WindowId> = {
	projects: 'finder',
	articles: 'safari',
	gallery: 'photos',
	contact: 'contact',
	skills: 'terminal',
	resume: 'resume',
	archive: 'finder',
};

export const SECTION_IDS = Object.keys(SECTION_WINDOWS) as SectionId[];

export interface SectionMeta {
	id: SectionId;
	windowId: WindowId;
	/** Window title. */
	title: string;
	/** Dock label, falling back to the title. */
	name: string;
	icon: string;
}

export type Sections = Partial<{ [K in SectionId]: SectionData[K] }>;

/**
 * Every `content/sections/*.json`, inlined at build time by Vite so the prerendered HTML and the
 * client bundle carry the same data. A deleted file is simply absent.
 */
const sectionModules = import.meta.glob('../../content/sections/*.json', {
	eager: true,
	import: 'default',
}) as Record<string, unknown>;

export function sectionIdFromPath(path: string): SectionId | null {
	const name = path.split('/').pop()?.replace(/\.json$/, '') ?? '';
	return (SECTION_IDS as string[]).includes(name) ? (name as SectionId) : null;
}

/** Files in `content/sections/` that do not match a known type — a typo, caught by the tests. */
export const unknownSectionFiles = Object.keys(sectionModules)
	.filter((path) => sectionIdFromPath(path) === null)
	.map((path) => path.split('/').pop() ?? path);

export const sectionData: Sections = {};
for (const [path, data] of Object.entries(sectionModules)) {
	const id = sectionIdFromPath(path);
	// The glob erases the per-file type, so the assignment needs widening back.
	if (id) (sectionData as Record<string, unknown>)[id] = resolveIcons(data);
}

/** The collection that must be non-empty for a section to count as present. */
function hasItems(id: SectionId): boolean {
	switch (id) {
		case 'projects':
			return (sectionData.projects?.projects.length ?? 0) > 0;
		case 'articles':
			return (sectionData.articles?.posts.length ?? 0) > 0;
		case 'gallery':
			return (sectionData.gallery?.photos.length ?? 0) > 0;
		case 'contact':
			return (sectionData.contact?.links.length ?? 0) > 0;
		case 'skills':
			return (sectionData.skills?.groups.length ?? 0) > 0;
		case 'resume':
			return Boolean(sectionData.resume?.file);
		case 'archive':
			return (sectionData.archive?.items.length ?? 0) > 0;
	}
}

/** A section is available when its file exists, is enabled, and actually has content. */
export function isSectionEnabled(id: SectionId): boolean {
	const section = sectionData[id];
	return Boolean(section && section.enabled !== false && hasItems(id));
}

function metaFor(id: SectionId): SectionMeta | null {
	const section = sectionData[id];
	if (!section) return null;
	return {
		id,
		windowId: SECTION_WINDOWS[id],
		title: section.title,
		name: section.name ?? section.title,
		icon: section.icon,
	};
}

/** Every enabled section, ordered by the dock list, with anything unlisted appended. */
export const sections: SectionMeta[] = [
	...menu.dock,
	...SECTION_IDS.filter((id) => !menu.dock.includes(id)),
]
	.filter((id, index, all) => all.indexOf(id) === index && isSectionEnabled(id))
	.map((id) => metaFor(id))
	.filter((meta): meta is SectionMeta => meta !== null);

export const sectionById = new Map(sections.map((meta) => [meta.id, meta]));

/** The enabled sections the dock asks for, in the order `menu.json` lists them. */
export const dockSections: SectionMeta[] = menu.dock
	.filter((id, index) => menu.dock.indexOf(id) === index && isSectionEnabled(id))
	.map((id) => metaFor(id))
	.filter((meta): meta is SectionMeta => meta !== null);

export function getSection(id: SectionId): SectionMeta | undefined {
	return sectionById.get(id);
}

export function hasSection(id: SectionId): boolean {
	return sectionById.has(id);
}

export function hasWindowSection(windowId: WindowId): boolean {
	return sections.some((meta) => meta.windowId === windowId);
}
