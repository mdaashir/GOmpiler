import { useLocation } from 'preact-iso';
import { useEffect, useState } from 'preact/hooks';

import { Calendar } from './Calendar';
import { docSections, hasSection, icon, menu, site } from '../content';
import { HOME } from '../lib/base';
import { openSection } from '../lib/open-section';
import { applyTheme, readTheme, toggleTheme, type Theme } from '../lib/theme';
import { useWindows } from '../lib/window-context';

/** "Thu Oct 15 11:07 PM" — the reference's menu-bar clock format. */
function formatClock(date: Date): string {
	return date
		.toLocaleString('en-US', {
			weekday: 'short',
			month: 'short',
			day: 'numeric',
			hour: 'numeric',
			minute: '2-digit',
		})
		.replace(/,/g, '');
}

export function Menubar() {
	const windowsApi = useWindows();
	const { path, route } = useLocation();
	const [theme, setTheme] = useState<Theme>('light');
	const [clock, setClock] = useState('');
	const [showCalendar, setShowCalendar] = useState(false);

	/** Both the Apple logo and the brand name clear the desktop. */
	const closeAllWindows = () => {
		windowsApi.closeAll();
		if (path !== HOME) route(HOME);
	};

	useEffect(() => {
		const initial = readTheme();
		setTheme(initial);
		applyTheme(initial);

		const tick = () => setClock(formatClock(new Date()));
		tick();
		const timer = window.setInterval(tick, 30_000);
		return () => window.clearInterval(timer);
	}, []);

	const onToggleTheme = () => setTheme(toggleTheme(theme));

	// A link whose section has no content is dropped along with the app itself.
	const links = menu.links.filter((link) => hasSection(link.section));

	/**
	 * A docs site has no portfolio sections, so the menu bar carries its sections instead —
	 * the same way the portfolio's menu bar carries Projects, Contact and Resume.
	 */
	const openDoc = (id: string) => {
		windowsApi.open('docs');
		window.location.hash = `#/${id}`;
	};

	return (
		<>
			<nav class='menubar' aria-label='Menu bar'>
				<div class='flex items-center gap-1'>
					<button
						type='button'
						class='menubar__item'
						aria-label={menu.closeAllLabel}
						title={menu.closeAllLabel}
						onClick={closeAllWindows}>
						<img src={icon('brand.logo')} alt='' class='h-4 w-4 dark:invert' />
					</button>
					<button
						type='button'
						class='menubar__item text-[0.9375rem] font-bold'
						title={menu.closeAllLabel}
						onClick={closeAllWindows}>
						{site.brand}
					</button>
					<ul class='menubar__links ml-3 flex items-center gap-5'>
						{links.map((link) => (
							<li key={link.section}>
								<button
									type='button'
									class='menubar__item'
									onClick={() => openSection(windowsApi, link.section)}>
									{link.label}
								</button>
							</li>
						))}
						{links.length === 0 &&
							docSections.map((section) => (
								<li key={section.id}>
									<button
										type='button'
										class='menubar__item'
										onClick={() => openDoc(section.id)}>
										{section.title}
									</button>
								</li>
							))}
					</ul>
				</div>

				<div class='flex items-center gap-3'>
					<ul class='flex items-center gap-2'>
						{menu.statusIcons.map((icon) => (
							<li key={icon.icon}>
								<img class='menubar__icon' src={icon.icon} alt={icon.label} title={icon.label} />
							</li>
						))}
					</ul>
					<button
						type='button'
						class='menubar__item menubar__item--icon'
						aria-label={menu.appearanceToggle.label}
						aria-pressed={theme === 'dark'}
						onClick={onToggleTheme}>
						<img class='menubar__icon' src={menu.appearanceToggle.icon} alt='' />
					</button>
					<button
						type='button'
						class='menubar__item clock'
						aria-haspopup='dialog'
						aria-expanded={showCalendar}
						onClick={() => setShowCalendar(true)}>
						{clock}
					</button>
				</div>
			</nav>
			{showCalendar && <Calendar onClose={() => setShowCalendar(false)} />}
		</>
	);
}
