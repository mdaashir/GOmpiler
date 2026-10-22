import { useEffect, useRef } from 'preact/hooks';

import { useContextMenu } from '../lib/context-menu';

export function ContextMenuHost() {
	const { state, close } = useContextMenu();
	const ref = useRef<HTMLDivElement>(null);

	// Dismiss on outside press, Escape, or window blur.
	useEffect(() => {
		if (!state) return;
		const onDown = (event: Event) => {
			if (ref.current && !ref.current.contains(event.target as Node)) close();
		};
		const onKey = (event: KeyboardEvent) => {
			if (event.key === 'Escape') close();
		};
		window.addEventListener('pointerdown', onDown, true);
		window.addEventListener('keydown', onKey);
		window.addEventListener('blur', close);
		return () => {
			window.removeEventListener('pointerdown', onDown, true);
			window.removeEventListener('keydown', onKey);
			window.removeEventListener('blur', close);
		};
	}, [state, close]);

	// Keep the menu inside the viewport.
	useEffect(() => {
		const el = ref.current;
		if (!state || !el) return;
		el.style.left = `${state.x}px`;
		el.style.top = `${state.y}px`;
		const rect = el.getBoundingClientRect();
		if (rect.right > window.innerWidth) {
			el.style.left = `${Math.max(8, window.innerWidth - rect.width - 8)}px`;
		}
		if (rect.bottom > window.innerHeight) {
			el.style.top = `${Math.max(8, window.innerHeight - rect.height - 8)}px`;
		}
	}, [state]);

	if (!state) return null;

	return (
		<div ref={ref} class='context-menu' role='menu'>
			{state.items.map((entry, index) =>
				'separator' in entry ? (
					<div key={`separator-${index}`} class='context-menu__separator' role='separator' />
				) : (
					<button
						key={`${entry.label}-${index}`}
						type='button'
						role='menuitem'
						class='context-menu__item'
						disabled={entry.disabled}
						onClick={() => {
							entry.onSelect?.();
							close();
						}}>
						<span>{entry.label}</span>
						{entry.hint && <span class='context-menu__hint'>{entry.hint}</span>}
					</button>
				)
			)}
		</div>
	);
}
