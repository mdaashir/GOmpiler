import { describe, expect, it } from 'vitest';

import { THEME_KEY, readTheme, saveTheme, toggleTheme } from '../lib/theme';

describe('theme store', () => {
	it('defaults to dark when nothing is stored, matching the reference', () => {
		window.localStorage.clear();
		expect(readTheme()).toBe('dark');
	});

	it('reads a stored preference', () => {
		window.localStorage.setItem(THEME_KEY, 'dark');
		expect(readTheme()).toBe('dark');
	});

	it('persists the toggled value and reflects it on <html>', () => {
		window.localStorage.setItem(THEME_KEY, 'light');
		const next = toggleTheme('light');

		expect(next).toBe('dark');
		expect(window.localStorage.getItem(THEME_KEY)).toBe('dark');
		expect(document.documentElement.classList.contains('dark')).toBe(true);

		toggleTheme('dark');
		expect(document.documentElement.classList.contains('dark')).toBe(false);
	});

	it('saveTheme ignores storage failures', () => {
		expect(() => saveTheme('light')).not.toThrow();
	});
});
