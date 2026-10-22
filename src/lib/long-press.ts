import { useRef } from 'preact/hooks';

const HOLD_MS = 500;
const MOVE_TOLERANCE_PX = 10;

export interface LongPressPoint {
	preventDefault: () => void;
	stopPropagation: () => void;
	clientX: number;
	clientY: number;
}

export interface LongPressHandlers {
	onTouchStart: (event: TouchEvent) => void;
	onTouchMove: (event: TouchEvent) => void;
	onTouchEnd: () => void;
	onTouchCancel: () => void;
	onClickCapture: (event: MouseEvent) => void;
}

/**
 * Touch long-press for a **container**, so lists stay free of per-item hooks: the element carrying
 * `data-long-press` under the finger is resolved with `closest()` and handed to `onLongPress`.
 *
 * The press is abandoned if the finger moves more than a few pixels (that is a scroll), if a second
 * finger lands, or if it is released early. The click that follows a fired long-press is suppressed,
 * so long-pressing an app tile opens its menu rather than the app.
 */
export function useLongPress(
	onLongPress: (target: HTMLElement, point: LongPressPoint) => void
): LongPressHandlers {
	const timer = useRef<number | null>(null);
	const origin = useRef<{ x: number; y: number } | null>(null);
	const fired = useRef(false);

	const cancel = () => {
		if (timer.current !== null) {
			window.clearTimeout(timer.current);
			timer.current = null;
		}
		origin.current = null;
	};

	const onTouchStart = (event: TouchEvent) => {
		if (event.touches.length !== 1) {
			cancel();
			return;
		}

		const touch = event.touches[0];
		const target = (event.target as HTMLElement | null)?.closest<HTMLElement>(
			'[data-long-press]'
		);
		if (!touch || !target) return;

		origin.current = { x: touch.clientX, y: touch.clientY };
		fired.current = false;

		timer.current = window.setTimeout(() => {
			timer.current = null;
			fired.current = true;
			onLongPress(target, {
				preventDefault: () => undefined,
				stopPropagation: () => undefined,
				clientX: touch.clientX,
				clientY: touch.clientY,
			});
		}, HOLD_MS);
	};

	const onTouchMove = (event: TouchEvent) => {
		const start = origin.current;
		const touch = event.touches[0];
		if (!start || !touch || event.touches.length !== 1) {
			cancel();
			return;
		}
		if (Math.hypot(touch.clientX - start.x, touch.clientY - start.y) > MOVE_TOLERANCE_PX) {
			cancel();
		}
	};

	const onTouchEnd = () => cancel();
	const onTouchCancel = () => cancel();

	const onClickCapture = (event: MouseEvent) => {
		if (!fired.current) return;
		fired.current = false;
		event.preventDefault();
		event.stopPropagation();
	};

	return { onTouchStart, onTouchMove, onTouchEnd, onTouchCancel, onClickCapture };
}
