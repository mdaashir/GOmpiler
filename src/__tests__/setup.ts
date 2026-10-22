import { afterEach } from 'vitest';
import { cleanup } from '@testing-library/preact';
import '@testing-library/jest-dom/vitest';

/*
 * jsdom has no `matchMedia`, but the shell queries it for hover/touch capability and the content
 * layer is unaffected. Reporting "no match" means a fine pointer with hover, i.e. the desktop.
 */
if (!window.matchMedia) {
	window.matchMedia = ((query: string) => ({
		matches: false,
		media: query,
		onchange: null,
		addEventListener: () => undefined,
		removeEventListener: () => undefined,
		addListener: () => undefined,
		removeListener: () => undefined,
		dispatchEvent: () => false,
	})) as unknown as typeof window.matchMedia;
}

afterEach(() => {
	cleanup();
});
