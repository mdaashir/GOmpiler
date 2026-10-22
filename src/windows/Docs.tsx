import { marked } from 'marked';
import { useEffect, useState } from 'preact/hooks';

import { doc, docMarkdown, docSections, type DocSection } from '../content';

/** A section id from `#/setup`, so a link straight into a page survives a reload. */
function sectionFromHash(): DocSection | null {
	if (typeof window === 'undefined') return null;
	const id = window.location.hash.replace(/^#\/?/, '');
	return docSections.find((section) => section.id === id) ?? null;
}

export function Docs() {
	const [activeId, setActiveId] = useState(sectionFromHash()?.id ?? docSections[0]?.id ?? '');
	const active = docSections.find((section) => section.id === activeId) ?? docSections[0];

	// The dock and the menu bar change the hash, so the reader follows it as well as leading it.
	useEffect(() => {
		const onHashChange = () => {
			const next = sectionFromHash();
			if (next) setActiveId(next.id);
		};

		window.addEventListener('hashchange', onHashChange);
		return () => window.removeEventListener('hashchange', onHashChange);
	}, []);

	useEffect(() => {
		if (typeof window === 'undefined' || !active) return;
		window.history.replaceState(null, '', `#/${active.id}`);
	}, [activeId]);

	const markdown = active ? docMarkdown(active.file) : null;
	const html = markdown ? marked.parse(markdown, { async: false }) : '';

	return (
		<div class='docs__body'>
			<aside class='docs__sidebar'>
				{doc.sidebarHeading && <h4 class='sidebar__heading'>{doc.sidebarHeading}</h4>}
				<ul class='space-y-0.5'>
					{docSections.map((section) => (
						<li key={section.id}>
							<button
								type='button'
								class={section.id === active?.id ? 'sidebar__item--active' : ''}
								onClick={() => setActiveId(section.id)}>
								{section.icon && <img src={section.icon} alt='' class='h-4 w-4' />}
								<span>{section.title}</span>
							</button>
						</li>
					))}
				</ul>
			</aside>

			<article class='docs__content' dangerouslySetInnerHTML={{ __html: html }} />
		</div>
	);
}
