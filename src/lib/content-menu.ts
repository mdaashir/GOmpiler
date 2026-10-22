import type { DesktopMenuItem } from '../content';
import type { MenuEntry } from './context-menu';

interface ResolvedItem {
	disabled?: boolean;
	onSelect?: () => void;
}

/**
 * Turns content-defined menu items into menu entries. `resolve` supplies the behaviour for an item
 * and may override `disabled` for state-dependent entries such as "Quit".
 */
export function contentMenu(
	items: DesktopMenuItem[],
	resolve: (item: DesktopMenuItem) => ResolvedItem | undefined
): MenuEntry[] {
	return items.map((item) => {
		if (item.separator) return { separator: true };

		const resolved = resolve(item) ?? {};

		return {
			label: item.label ?? '',
			hint: item.hint,
			disabled: resolved.disabled ?? item.disabled ?? false,
			onSelect: resolved.onSelect,
		};
	});
}
