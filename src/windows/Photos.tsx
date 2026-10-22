import { useState } from 'preact/hooks';

import { sectionData } from '../content';

export function Photos() {
	const section = sectionData.gallery;
	const [active, setActive] = useState(section?.albums[0]?.title ?? '');

	if (!section) return null;

	return (
		<div class='photos__body'>
			{section.albums.length > 0 && (
				<aside class='photos__sidebar'>
					{section.sidebarHeading && (
						<h4 class='sidebar__heading'>{section.sidebarHeading}</h4>
					)}
					<ul class='space-y-0.5'>
						{section.albums.map((album) => (
							<li key={album.title}>
								<button
									type='button'
									class={album.title === active ? 'sidebar__item--active' : ''}
									onClick={() => setActive(album.title)}>
									<img src={album.icon} alt='' class='h-4 w-4' />
									<span>{album.title}</span>
								</button>
							</li>
						))}
					</ul>
				</aside>
			)}

			<div class='photos__grid'>
				{section.photos.map((photo) => (
					<img key={photo.src} src={photo.src} alt={photo.alt ?? ''} loading='lazy' />
				))}
			</div>
		</div>
	);
}
