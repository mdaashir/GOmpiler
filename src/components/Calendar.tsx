import { useEffect, useMemo, useRef, useState } from 'preact/hooks';

const WEEKDAYS = ['S', 'M', 'T', 'W', 'T', 'F', 'S'];
const MONTHS = [
	'January',
	'February',
	'March',
	'April',
	'May',
	'June',
	'July',
	'August',
	'September',
	'October',
	'November',
	'December',
];

interface DayCell {
	date: Date;
	muted: boolean;
}

export function Calendar({ onClose }: { onClose: () => void }) {
	const now = new Date();
	const [cursor, setCursor] = useState(() => new Date(now.getFullYear(), now.getMonth(), 1));
	const [clock, setClock] = useState(() => new Date());
	const ref = useRef<HTMLDivElement>(null);

	useEffect(() => {
		const timer = window.setInterval(() => setClock(new Date()), 30_000);
		return () => window.clearInterval(timer);
	}, []);

	useEffect(() => {
		const onDown = (event: Event) => {
			if (ref.current && !ref.current.contains(event.target as Node)) onClose();
		};
		const onKey = (event: KeyboardEvent) => {
			if (event.key === 'Escape') onClose();
		};
		window.addEventListener('pointerdown', onDown, true);
		window.addEventListener('keydown', onKey);
		return () => {
			window.removeEventListener('pointerdown', onDown, true);
			window.removeEventListener('keydown', onKey);
		};
	}, [onClose]);

	const cells = useMemo<DayCell[]>(() => {
		const year = cursor.getFullYear();
		const month = cursor.getMonth();
		const leading = new Date(year, month, 1).getDay();
		const daysInMonth = new Date(year, month + 1, 0).getDate();

		const out: DayCell[] = [];
		for (let i = leading; i > 0; i -= 1) {
			out.push({ date: new Date(year, month, 1 - i), muted: true });
		}
		for (let day = 1; day <= daysInMonth; day += 1) {
			out.push({ date: new Date(year, month, day), muted: false });
		}
		const trailing = (7 - (out.length % 7)) % 7;
		for (let day = 1; day <= trailing; day += 1) {
			out.push({ date: new Date(year, month + 1, day), muted: true });
		}
		return out;
	}, [cursor]);

	const isToday = (date: Date) => date.toDateString() === new Date().toDateString();

	const shiftMonth = (delta: number) =>
		setCursor((current) => new Date(current.getFullYear(), current.getMonth() + delta, 1));

	return (
		<div ref={ref} class='calendar' role='dialog' aria-label='Date and time'>
			<div class='calendar__header'>
				<button
					type='button'
					class='calendar__nav'
					aria-label='Previous month'
					onClick={() => shiftMonth(-1)}>
					‹
				</button>
				<span class='calendar__title'>
					{MONTHS[cursor.getMonth()]} {cursor.getFullYear()}
				</span>
				<button
					type='button'
					class='calendar__nav'
					aria-label='Next month'
					onClick={() => shiftMonth(1)}>
					›
				</button>
			</div>

			<div class='calendar__weekdays'>
				{WEEKDAYS.map((day, index) => (
					<span key={`${day}-${index}`}>{day}</span>
				))}
			</div>

			<div class='calendar__grid'>
				{cells.map((cell) => (
					<span
						key={cell.date.toISOString()}
						class={`calendar__cell${
							cell.muted ? ' calendar__cell--muted' : ''
						}${isToday(cell.date) ? ' calendar__cell--today' : ''}`}>
						{cell.date.getDate()}
					</span>
				))}
			</div>

			<p class='calendar__time'>
				{clock.toLocaleString('en-US', {
					weekday: 'long',
					month: 'long',
					day: 'numeric',
					hour: 'numeric',
					minute: '2-digit',
				})}
			</p>
		</div>
	);
}
