/**
 * Documentation content: `content/doc.json` describes the sidebar, and `content/docs/*.md` holds the
 * prose. Markdown is bundled at build time, so a docs site is fully prerendered.
 */
import docFile from '../../content/doc.json';

import { resolveIcons } from './icons';

export interface DocSection {
	id: string;
	title: string;
	/** File name inside `content/docs/`. */
	file: string;
	icon?: string;
}

export interface DocConfig {
	enabled?: boolean;
	title: string;
	name?: string;
	subtitle?: string;
	repo?: string;
	sidebarHeading?: string;
	sections: DocSection[];
}

const markdown = import.meta.glob('../../content/docs/*.md', {
	eager: true,
	query: '?raw',
	import: 'default',
}) as Record<string, string>;

export const doc = resolveIcons(docFile as DocConfig);

/** The Markdown for a section, or null when the file is missing. */
export function docMarkdown(file: string): string | null {
	const entry = Object.entries(markdown).find(([path]) => path.endsWith(`/${file}`));
	return entry ? entry[1] : null;
}

/** Only sections whose Markdown exists — a missing file leaves no empty page in the sidebar. */
export const docSections: DocSection[] = (doc.sections ?? []).filter(
	(section) => docMarkdown(section.file) !== null
);

export const hasDoc = doc.enabled !== false && docSections.length > 0;
