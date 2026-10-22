import { render, screen } from '@testing-library/preact';
import { act } from 'preact/test-utils';
import { describe, expect, it } from 'vitest';

import { INITIAL_Z_INDEX } from '../lib/windows';
import { WindowProvider, useWindows, type WindowsApi } from '../lib/window-context';

let api: WindowsApi;

function Probe() {
	api = useWindows();
	return <div data-testid='probe'>{api.windows.finder.isOpen ? 'open' : 'closed'}</div>;
}

function mount() {
	return render(
		<WindowProvider>
			<Probe />
		</WindowProvider>
	);
}

describe('window store', () => {
	it('starts with every window closed', () => {
		mount();
		expect(screen.getByTestId('probe').textContent).toBe('closed');
	});

	it('opens a window, carries its payload and raises its z-index', () => {
		mount();

		act(() => api.open('finder', { location: 'work' }));

		expect(screen.getByTestId('probe').textContent).toBe('open');
		expect(api.windows.finder.data?.location).toBe('work');
		expect(api.windows.finder.zIndex).toBe(INITIAL_Z_INDEX + 1);
	});

	it('stacks a newer window above an older one, and focus re-raises', () => {
		mount();

		act(() => api.open('finder'));
		const finderZ = api.windows.finder.zIndex;

		act(() => api.open('safari'));
		const safariZ = api.windows.safari.zIndex;
		expect(safariZ).toBeGreaterThan(finderZ);

		act(() => api.focus('finder'));
		expect(api.windows.finder.zIndex).toBeGreaterThan(safariZ);
	});

	it('ignores focus for a closed window', () => {
		mount();
		act(() => api.focus('photos'));
		expect(api.windows.photos.zIndex).toBe(INITIAL_Z_INDEX);
	});

	it('toggles closed and clears the payload on close', () => {
		mount();

		act(() => api.toggle('terminal'));
		expect(api.windows.terminal.isOpen).toBe(true);

		act(() => api.toggle('terminal'));
		expect(api.windows.terminal.isOpen).toBe(false);
	});

	it('closeAll resets every window', () => {
		mount();

		act(() => api.open('finder'));
		act(() => api.open('contact'));
		act(() => api.closeAll());

		expect(api.windows.finder.isOpen).toBe(false);
		expect(api.windows.contact.isOpen).toBe(false);
	});
});
