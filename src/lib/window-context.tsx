import { createContext, type ComponentChildren } from 'preact';
import { useCallback, useContext, useMemo, useReducer } from 'preact/hooks';

import {
	INITIAL_Z_INDEX,
	WINDOW_CONFIG,
	WINDOW_IDS,
	type WindowId,
	type WindowPayload,
	type WindowState,
} from './windows';

interface State {
	windows: Record<WindowId, WindowState>;
	topZ: number;
}

type Action =
	| { type: 'open'; id: WindowId; data?: WindowPayload }
	| { type: 'close'; id: WindowId }
	| { type: 'focus'; id: WindowId }
	| { type: 'toggle'; id: WindowId; data?: WindowPayload }
	| { type: 'closeAll' };

function reducer(state: State, action: Action): State {
	const current = (id: WindowId) => state.windows[id];

	switch (action.type) {
		case 'open': {
			const topZ = state.topZ + 1;
			return {
				topZ,
				windows: {
					...state.windows,
					[action.id]: {
						isOpen: true,
						zIndex: topZ,
						data: action.data ?? current(action.id).data,
					},
				},
			};
		}
		case 'toggle': {
			if (current(action.id).isOpen) {
				return {
					...state,
					windows: { ...state.windows, [action.id]: { ...current(action.id), isOpen: false } },
				};
			}
			return reducer(state, { type: 'open', id: action.id, data: action.data });
		}
		case 'close':
			return {
				...state,
				windows: {
					...state.windows,
					[action.id]: { ...current(action.id), isOpen: false, data: null },
				},
			};
		case 'focus': {
			if (!current(action.id).isOpen) return state;
			const topZ = state.topZ + 1;
			return {
				topZ,
				windows: {
					...state.windows,
					[action.id]: { ...current(action.id), zIndex: topZ },
				},
			};
		}
		case 'closeAll': {
			const windows = { ...state.windows };
			for (const id of WINDOW_IDS) {
				windows[id] = { ...windows[id], isOpen: false, data: null };
			}
			return { ...state, windows };
		}
		default:
			return state;
	}
}

const initialState: State = { windows: WINDOW_CONFIG, topZ: INITIAL_Z_INDEX };

export interface WindowsApi {
	windows: Record<WindowId, WindowState>;
	open: (id: WindowId, data?: WindowPayload) => void;
	close: (id: WindowId) => void;
	toggle: (id: WindowId, data?: WindowPayload) => void;
	focus: (id: WindowId) => void;
	closeAll: () => void;
}

const WindowContext = createContext<WindowsApi | null>(null);

export function WindowProvider({ children }: { children: ComponentChildren }) {
	const [state, dispatch] = useReducer(reducer, initialState);

	const open = useCallback((id: WindowId, data?: WindowPayload) => dispatch({ type: 'open', id, data }), []);
	const close = useCallback((id: WindowId) => dispatch({ type: 'close', id }), []);
	const toggle = useCallback(
		(id: WindowId, data?: WindowPayload) => dispatch({ type: 'toggle', id, data }),
		[]
	);
	const focus = useCallback((id: WindowId) => dispatch({ type: 'focus', id }), []);
	const closeAll = useCallback(() => dispatch({ type: 'closeAll' }), []);

	const value = useMemo<WindowsApi>(
		() => ({ windows: state.windows, open, close, toggle, focus, closeAll }),
		[state.windows, open, close, toggle, focus, closeAll]
	);

	return <WindowContext.Provider value={value}>{children}</WindowContext.Provider>;
}

export function useWindows(): WindowsApi {
	const context = useContext(WindowContext);
	if (!context) throw new Error('useWindows must be used inside <WindowProvider>');
	return context;
}
