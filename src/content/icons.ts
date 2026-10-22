/**
 * The icon registry.
 *
 * Icon artwork is bundled at build time from icon packages — nothing is downloaded and no asset
 * files need keeping in sync. `content/icons.json` names the icon for each role
 * (`menubar.wifi`, `social.github`, …), so changing an icon is a JSON edit.
 */
import fileText from 'lucide-static/icons/file-text.svg?raw';
import folder from 'lucide-static/icons/folder.svg?raw';
import info from 'lucide-static/icons/info.svg?raw';
import moon from 'lucide-static/icons/moon.svg?raw';
import search from 'lucide-static/icons/search.svg?raw';
import trash from 'lucide-static/icons/trash-2.svg?raw';
import user from 'lucide-static/icons/user.svg?raw';
import wifi from 'lucide-static/icons/wifi.svg?raw';

import { siGithub, siGmail, siX } from 'simple-icons';

import iconFile from '../../content/icons.json';

const asDataUri = (svg: string): string =>
	`data:image/svg+xml;charset=utf-8,${encodeURIComponent(svg)}`;

const brand = (icon: { path: string }, fill = '#fff'): string =>
	asDataUri(
		`<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24"><path fill="${fill}" d="${icon.path}"/></svg>`
	);

/** Every icon available by name, bundled into the bundle rather than fetched. */
export const inlineIcons: Record<string, string> = {
	wifi: asDataUri(wifi),
	search: asDataUri(search),
	user: asDataUri(user),
	moon: asDataUri(moon),
	folder: asDataUri(folder),
	file: asDataUri(fileText),
	info: asDataUri(info),
	trash: asDataUri(trash),
	github: brand(siGithub),
	x: brand(siX),
	gmail: brand(siGmail),
};

type IconGroups = Record<string, Record<string, string>>;

const groups = iconFile as IconGroups;

/** A configured value is either an icon name (bundled) or a path to an image in `public/`. */
function resolveValue(value: string): string {
	return inlineIcons[value] ?? value;
}

/**
 * Content paths are written as `/images/…`, but a docs site is served from `/<repo>/`. Prefixing
 * them with Vite's base keeps every asset resolving on a project Pages site; on the root site the
 * base is `/`, so nothing changes.
 */
function withBase(path: string): string {
	if (!path.startsWith('/') || path.startsWith('//')) return path;
	return `${import.meta.env.BASE_URL}${path.slice(1)}`;
}

/** Flat role → source map, e.g. `{ 'menubar.wifi': 'data:image/svg+xml,…' }`. */
export const icons: Record<string, string> = Object.fromEntries(
	Object.entries(groups).flatMap(([group, entries]) =>
		Object.entries(entries).map(([name, value]) => [
			`${group}.${name}`,
			withBase(resolveValue(value)),
		])
	)
);

/** Every role in the registry, sorted — used by the content tests. */
export const iconRoles: string[] = Object.keys(icons).sort();

/**
 * Resolves a role to its source. Anything that is not a role (already a path or a data URI) is
 * returned unchanged, so a missing role never produces a broken image.
 */
export function icon(roleOrPath: string): string {
	if (roleOrPath in icons) return icons[roleOrPath];
	return resolveValue(roleOrPath);
}

/**
 * Walks loaded content and resolves any string that is a role name or a bundled icon name. Runs once
 * at load time so components keep rendering plain sources and never need to know about the registry.
 */
export function resolveIcons<T>(value: T): T {
	if (typeof value === 'string') return withBase(icon(value)) as unknown as T;
	if (Array.isArray(value)) return value.map((entry) => resolveIcons(entry)) as T;
	if (value && typeof value === 'object') {
		return Object.fromEntries(
			Object.entries(value as Record<string, unknown>).map(([key, entry]) => [
				key,
				resolveIcons(entry),
			])
		) as T;
	}
	return value;
}
