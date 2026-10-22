import { render } from '@testing-library/preact';
import { LocationProvider } from 'preact-iso';
import { describe, expect, it } from 'vitest';

import { Desktop } from '../components/Desktop';
import { doc, docSections } from '../content';
import { ContextMenuProvider } from '../lib/context-menu';
import { WindowProvider } from '../lib/window-context';

describe('docs site', () => {
	it('finds the configured sections', () => {
		expect(docSections.length).toBeGreaterThan(0);
		expect(docSections[0].file).toMatch(/\.md$/);
	});

	it('renders the documentation window with the first page', () => {
		const { container } = render(
			<LocationProvider>
				<ContextMenuProvider>
					<WindowProvider>
						<Desktop />
					</WindowProvider>
				</ContextMenuProvider>
			</LocationProvider>
		);

		expect(container.querySelector('.docs')).toBeTruthy();
		expect(container.querySelector('.docs__sidebar')?.textContent).toContain(
			docSections[0].title
		);
		expect(container.querySelector('.docs__content h1')?.textContent).toBeTruthy();
	});
});
