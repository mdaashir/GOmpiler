import type { FileNode, LocationId } from '../content/types';

export type WindowId =
	| 'finder'
	| 'safari'
	| 'photos'
	| 'contact'
	| 'terminal'
	| 'resume'
	| 'txtfile'
	| 'imgfile'
	| 'docs';

export interface WindowPayload {
	location?: LocationId;
	file?: FileNode;
}

export interface WindowState {
	isOpen: boolean;
	zIndex: number;
	data: WindowPayload | null;
}

export const INITIAL_Z_INDEX = 1000;

function closed(): WindowState {
	return { isOpen: false, zIndex: INITIAL_Z_INDEX, data: null };
}

/**
 * Every window the app knows about, whether or not a section currently drives it. Rendering is gated
 * on the section being enabled, so the reducer below stays total and type-safe either way.
 */
export const WINDOW_CONFIG: Record<WindowId, WindowState> = {
	finder: closed(),
	contact: closed(),
	resume: closed(),
	safari: closed(),
	photos: closed(),
	terminal: closed(),
	txtfile: closed(),
	imgfile: closed(),
	/** Documentation sites open their reader immediately; they have nothing else on the desktop. */
	docs: { isOpen: true, zIndex: INITIAL_Z_INDEX, data: null },
};

export const WINDOW_IDS = Object.keys(WINDOW_CONFIG) as WindowId[];
