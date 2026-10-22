import { interpolate, site } from '../content';

export interface HeadElement {
	type: string;
	props: Record<string, string>;
}

export interface Head {
	title: string;
	elements: HeadElement[];
}

export function getHead(url = '/'): Head {
	const path = url.split('?')[0].split('#')[0].replace(/\/+$/, '') || '/';
	const isHome = path === '/';

	const tokens = { name: site.name };
	const title = interpolate(isHome ? site.title : site.notFound.title, tokens);
	const description = isHome ? site.description : site.notFound.description;

	return {
		title,
		elements: [
			{ type: 'meta', props: { name: 'description', content: description } },
			{ type: 'meta', props: { name: 'theme-color', content: site.themeColor } },
			{ type: 'meta', props: { property: 'og:type', content: 'website' } },
			{ type: 'meta', props: { property: 'og:title', content: title } },
			{ type: 'meta', props: { property: 'og:description', content: description } },
			{ type: 'meta', props: { property: 'og:url', content: `${site.url}${path}` } },
			{ type: 'meta', props: { name: 'twitter:card', content: 'summary_large_image' } },
			{ type: 'link', props: { rel: 'canonical', href: `${site.url}${path}` } },
		],
	};
}
