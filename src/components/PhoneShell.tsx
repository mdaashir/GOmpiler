import type { ComponentChildren } from 'preact';
import { useEffect, useState } from 'preact/hooks';

import { phone, profile, sectionById, site, type LocationId, type PhoneApp, type SectionId } from '../content';
import { Contact } from '../windows/Contact';
import { Finder } from '../windows/Finder';
import { Photos } from '../windows/Photos';
import { Resume } from '../windows/Resume';
import { Safari } from '../windows/Safari';
import { Terminal } from '../windows/Terminal';

interface AppView {
	label: string;
	section: SectionId;
	location?: LocationId;
}

const RENDERERS: Partial<Record<SectionId, (view: AppView, query: string) => ComponentChildren>> = {
	projects: (view, query) => <Finder initialLocation={view.location} query={query} />,
	articles: () => <Safari />,
	gallery: () => <Photos />,
	contact: () => <Contact />,
	skills: () => <Terminal />,
	resume: () => <Resume />,
};

/** Only the Work screens carry a search field in the design (pages 21-22). */
const SEARCHABLE: SectionId[] = ['projects'];

/** "12:08 AM", as the iOS status bar shows it. */
function PhoneClock() {
	const [clock, setClock] = useState('');

	useEffect(() => {
		const tick = () =>
			setClock(new Date().toLocaleTimeString('en-US', { hour: 'numeric', minute: '2-digit' }));
		tick();
		const timer = window.setInterval(tick, 30_000);
		return () => window.clearInterval(timer);
	}, []);

	return <span>{clock}</span>;
}

/** The iOS status bar, reused on the home screen and inside apps. */
function StatusBar({ dark = false }: { dark?: boolean }) {
	return (
		<header class={`phone__status${dark ? ' phone__status--dark' : ''}`}>
			<PhoneClock />
			<span class='phone__notch' aria-hidden='true' />
			<span class='phone__status-icons'>
				<svg viewBox='0 0 20 12' class='h-3 w-5' aria-hidden='true'>
					<rect x='0' y='7' width='3' height='5' rx='1' fill='#fff' />
					<rect x='4.5' y='5' width='3' height='7' rx='1' fill='#fff' />
					<rect x='9' y='2.5' width='3' height='9.5' rx='1' fill='#fff' />
					<rect x='13.5' y='0' width='3' height='12' rx='1' fill='#fff' fill-opacity='0.45' />
				</svg>
				<svg viewBox='0 0 16 12' class='h-3 w-4' aria-hidden='true'>
					<path
						d='M8 10.4 6.1 8.4a2.8 2.8 0 0 1 3.8 0zM4.3 6.6 2.6 4.8a7.8 7.8 0 0 1 10.8 0l-1.7 1.8a5.4 5.4 0 0 0-7.4 0z'
						fill='#fff'
					/>
				</svg>
				<BatteryIcon />
			</span>
		</header>
	);
}

function BatteryIcon() {
	return (
		<svg viewBox='0 0 28 14' class='h-3.5 w-7 shrink-0' aria-hidden='true'>
			<rect
				x='0.75'
				y='0.75'
				width='23'
				height='12.5'
				rx='4'
				fill='none'
				stroke='#fff'
				stroke-opacity='0.85'
				stroke-width='1.5'
			/>
			<rect x='2.5' y='2.5' width='14' height='9' rx='2.5' fill='#fff' />
			<path d='M25.5 5.2v3.6a2 2 0 0 0 0-3.6z' fill='#fff' fill-opacity='0.85' />
		</svg>
	);
}

export function PhoneShell() {
	const [opened, setOpened] = useState<AppView | null>(null);
	const [searching, setSearching] = useState(false);
	const [query, setQuery] = useState('');
	const [appQuery, setAppQuery] = useState('');

	const matches = (app: PhoneApp) =>
		!query || app.label.toLowerCase().includes(query.trim().toLowerCase());

	const grid = phone.grid.filter(matches);
	const dock = phone.dock.filter(matches);

	const open = (app: PhoneApp) => {
		if (!app.section) return; // decorative iOS icons from the design have no content behind them
		setOpened({ label: app.label, section: app.section, location: app.location });
		setSearching(false);
		setQuery('');
		setAppQuery('');
	};

	const close = () => {
		setOpened(null);
		setQuery('');
		setAppQuery('');
	};

	const title = opened
		? (opened.location ? titleForLocation(opened.location) : sectionById.get(opened.section)?.title) ??
			opened.label
		: '';

	return (
		<div class='phone-shell' style={{ backgroundImage: `url(${phone.wallpaper})` }}>
			<StatusBar />

			<div class='phone__home'>
				{/* Names the site, so a visitor landing on a phone knows this is a portfolio. */}
				{site.brand && (
					<div class='phone__identity'>
						<strong>{site.brand}</strong>
						<span>{site.name}</span>
					</div>
				)}

				<div class='phone__grid'>
					{grid.map((app) => (
						<button
							key={app.label}
							type='button'
							class={`phone__app${app.section ? '' : ' phone__app--decorative'}`}
							aria-label={app.label}
							aria-disabled={app.section ? undefined : 'true'}
							onClick={() => open(app)}>
							<img
								src={app.icon}
								alt=''
								loading='lazy'
								onError={(event) => {
									// The hand-authored SVGs are the fallback if a PNG ever fails to load.
									const image = event.currentTarget;
									if (image.dataset.fallback) return;
									image.dataset.fallback = 'true';
									image.src = image.src.replace(/\.png$/, '.svg');
								}}
							/>
							<span>{app.label}</span>
						</button>
					))}
				</div>

				{searching ? (
					<div class='phone__search phone__search--open'>
						<SearchGlyph />
						<input
							type='search'
							autofocus
							value={query}
							placeholder={phone.search.placeholder}
							aria-label={phone.search.placeholder}
							onInput={(event) => setQuery((event.target as HTMLInputElement).value)}
						/>
						<button type='button' class='phone__search-close' onClick={() => { setSearching(false); setQuery(''); }}>
							Cancel
						</button>
					</div>
				) : (
					<button type='button' class='phone__search' onClick={() => setSearching(true)}>
						<SearchGlyph />
						<span>{phone.search.placeholder}</span>
					</button>
				)}

				<nav class='phone__dock' aria-label='Dock'>
					{dock.map((app) => (
						<button
							key={app.label}
							type='button'
							class='phone__dock-icon'
							aria-label={app.label}
							onClick={() => open(app)}>
							<img src={app.icon} alt='' loading='lazy' />
						</button>
					))}
				</nav>

				<div class='phone__home-indicator' aria-hidden='true' />
			</div>

			{opened && (
				<div class='phone__app-view' role='dialog' aria-label={title}>
					<StatusBar dark />
					<header class='phone__app-nav'>
						<button type='button' class='phone__nav-back' onClick={close}>
							‹ Go back
						</button>
						<span class='phone__nav-title'>{title}</span>
						<button type='button' class='phone__nav-cancel' onClick={close}>
							☰ Cancel
						</button>
					</header>

					{SEARCHABLE.includes(opened.section) && (
						<div class='phone__app-search'>
							<SearchGlyph />
							<input
								type='search'
								value={appQuery}
								placeholder={phone.search.placeholder}
								aria-label={phone.search.placeholder}
								onInput={(event) => setAppQuery((event.target as HTMLInputElement).value)}
							/>
							<span class='phone__app-search-mic' aria-hidden='true'>
								🎤
							</span>
						</div>
					)}

					<div class='phone__app-body'>{RENDERERS[opened.section]?.(opened, appQuery)}</div>

					<nav class='phone__tabs' aria-label='Tabs'>
						{phone.tabs.map((tab) => (
							<button
								key={tab.label}
								type='button'
								class={`phone__tab${tab.location === opened.location ? ' phone__tab--active' : ''}`}
								onClick={() => open(tab)}>
								<img src={tab.icon} alt='' />
								<span>{tab.label}</span>
							</button>
						))}
					</nav>
					<div class='phone__home-indicator' aria-hidden='true' />
				</div>
			)}
		</div>
	);
}

/** The tab title shows the folder being viewed, as the design's nav bar does. */
function titleForLocation(location: LocationId): string | undefined {
	const names: Record<LocationId, string> = {
		work: 'Work',
		about: 'About Me',
		resume: 'Resume',
		archive: 'Trash',
	};
	return names[location];
}

function SearchGlyph() {
	return (
		<svg viewBox='0 0 16 16' class='h-4 w-4 shrink-0' aria-hidden='true'>
			<circle cx='7' cy='7' r='4.6' fill='none' stroke='currentColor' stroke-width='1.8' />
			<path d='M10.6 10.6 14 14' stroke='currentColor' stroke-width='1.8' stroke-linecap='round' />
		</svg>
	);
}
