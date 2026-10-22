import { createContext, type ComponentChildren } from 'preact';
import { useCallback, useContext, useMemo, useState } from 'preact/hooks';

export interface MenuAction {
	label: string;
	/** Optional right-aligned hint, e.g. "⌘I". */
	hint?: string;
	disabled?: boolean;
	onSelect?: () => void;
}

export interface MenuSeparator {
	separator: true;
}

export type MenuEntry = MenuAction | MenuSeparator;

/** Structural shape shared by DOM and Preact mouse events. */
export interface MenuTriggerEvent {
	preventDefault: () => void;
	stopPropagation: () => void;
	clientX: number;
	clientY: number;
}

interface MenuState {
	x: number;
	y: number;
	items: MenuEntry[];
}

interface ContextMenuApi {
	state: MenuState | null;
	openAt: (event: MenuTriggerEvent, items: MenuEntry[]) => void;
	close: () => void;
}

const ContextMenuContext = createContext<ContextMenuApi | null>(null);

export function ContextMenuProvider({ children }: { children: ComponentChildren }) {
	const [state, setState] = useState<MenuState | null>(null);

	const close = useCallback(() => setState(null), []);
	const openAt = useCallback((event: MenuTriggerEvent, items: MenuEntry[]) => {
		event.preventDefault();
		event.stopPropagation();
		setState({ x: event.clientX, y: event.clientY, items });
	}, []);

	const value = useMemo<ContextMenuApi>(
		() => ({ state, openAt, close }),
		[state, openAt, close]
	);

	return <ContextMenuContext.Provider value={value}>{children}</ContextMenuContext.Provider>;
}

export function useContextMenu(): ContextMenuApi {
	const context = useContext(ContextMenuContext);
	if (!context) throw new Error('useContextMenu must be used inside <ContextMenuProvider>');
	return context;
}
