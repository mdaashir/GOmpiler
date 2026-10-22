import { sectionData } from '../content';

export function Contact() {
	const section = sectionData.contact;
	if (!section) return null;

	return (
		<div class='contact__body'>
			<div class='flex items-center gap-4'>
				{section.avatar && (
					<img class='contact__avatar' src={section.avatar} alt='' loading='lazy' />
				)}
				<div>
					{section.heading && <h3 class='text-lg font-semibold'>{section.heading}</h3>}
					{section.blurb && (
						<p class='text-sm text-label-2 dark:text-dark-100'>{section.blurb}</p>
					)}
				</div>
			</div>

			<ul class='contact__tiles'>
				{section.links.map((link) => (
					<li key={link.text}>
						<a
							class='contact__tile'
							href={link.link}
							target='_blank'
							rel='noopener noreferrer'
							style={link.bg ? { backgroundColor: link.bg } : undefined}>
							<img src={link.icon} alt='' loading='lazy' />
							<span>{link.text}</span>
						</a>
					</li>
				))}
			</ul>
		</div>
	);
}
