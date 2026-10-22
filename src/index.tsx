import { LocationProvider, Route, Router, hydrate, prerender as ssr } from 'preact-iso';

import { Desktop } from './components/Desktop';
import { site } from './content';
import { HOME } from './lib/base';
import { ContextMenuProvider } from './lib/context-menu';
import { WindowProvider } from './lib/window-context';
import { getHead } from './lib/seo';
import './style.css';

function NotFound() {
	return (
		<>
			<Desktop />
			<div class='abs-center col-center gap-2 rounded-xl bg-label/80 px-8 py-6 text-center text-white shadow-2xl backdrop-blur'>
				<h1 class='text-xl font-semibold'>{site.ui.notFoundHeading}</h1>
				<p class='text-sm opacity-80'>{site.ui.notFoundBody}</p>
				<a class='mt-1 text-sm text-system-blue underline' href={HOME}>
					{site.ui.notFoundBack}
				</a>
			</div>
		</>
	);
}

export function App() {
	return (
		<LocationProvider>
			<ContextMenuProvider>
				<WindowProvider>
					<main>
						<Router>
							<Route path='/' component={Desktop} />
							{/* A project site deploys at `/<repo>/`; prerendering still runs at `/`. */}
							{HOME !== '/' && <Route path={HOME} component={Desktop} />}
							<Route default component={NotFound} />
						</Router>
					</main>
				</WindowProvider>
			</ContextMenuProvider>
		</LocationProvider>
	);
}

if (typeof window !== 'undefined') {
	hydrate(<App />, document.getElementById('app')!);
}

export async function prerender(data: { url?: string }) {
	const result = await ssr(<App />);
	return { ...result, head: getHead(data?.url ?? '/') };
}
