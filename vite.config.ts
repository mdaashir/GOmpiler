import { defineConfig } from 'vite';
import preact from '@preact/preset-vite';
import tailwindcss from '@tailwindcss/vite';
import { copyFileSync, existsSync, writeFileSync } from 'node:fs';
import { resolve } from 'node:path';
import { fileURLToPath } from 'node:url';

import site from './content/site.json';

const rootDir = fileURLToPath(new URL('.', import.meta.url));

const TITLE = site.title.replace(/\{name\}/g, site.name);

/** Replaces the placeholders in index.html with values from content/site.json. */
function htmlFromContent() {
	return {
		name: 'portfolio-html-content',
		transformIndexHtml(html: string) {
			return html
				.replaceAll('__TITLE__', TITLE)
				.replaceAll('__FAVICON__', site.favicon)
				.replaceAll('__THEME_KEY__', 'theme')
				.replaceAll('__THEME_DEFAULT__', site.theme.default);
		},
	};
}

/** Emits the GitHub Pages extras: a root 404.html and sitemap.xml. */
function siteExtras() {
	return {
		name: 'portfolio-site-extras',
		apply: 'build' as const,
		writeBundle() {
			const dist = resolve(rootDir, 'dist');
			const notFoundSource = resolve(dist, '404', 'index.html');
			if (existsSync(notFoundSource)) {
				copyFileSync(notFoundSource, resolve(dist, '404.html'));
			}

			const sitemap = `<?xml version="1.0" encoding="UTF-8"?>
<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">
\t<url>
\t\t<loc>${site.url}/</loc>
\t</url>
</urlset>
`;
			writeFileSync(resolve(dist, 'sitemap.xml'), sitemap);
		},
	};
}

export default defineConfig({
	plugins: [
		tailwindcss(),
		htmlFromContent(),
		preact({
			prerender: {
				enabled: true,
				renderTarget: '#app',
				additionalPrerenderRoutes: ['/404'],
				previewMiddlewareEnabled: true,
				previewMiddlewareFallback: '/404',
			},
		}),
		siteExtras(),
	],
	build: {
		outDir: 'dist',
	},
	/** A project Pages site is served from `/<repo>/`; the deploy workflow sets PAGES_BASE. */
	base: process.env.PAGES_BASE ?? '/',
});
