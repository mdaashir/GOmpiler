import { interpolate, sectionData } from '../content';

export function Terminal() {
	const section = sectionData.skills;
	if (!section) return null;

	const total = section.groups.length;

	return (
		<div class='terminal__body'>
			<p>
				{section.prompt && <span class='font-bold'>{section.prompt}</span>} {section.command}
			</p>

			{section.columns && (
				<div class='terminal__row mt-5'>
					<span class='font-semibold'>{section.columns[0]}</span>
					<span>{section.columns[1]}</span>
				</div>
			)}

			<ul class='terminal__list'>
				{section.groups.map((group) => (
					<li key={group.category} class='flex items-start gap-3'>
						<span aria-hidden='true'>✓</span>
						<span class='w-24 flex-none font-semibold'>{group.category}</span>
						<span>{group.items.join(', ')}</span>
					</li>
				))}
			</ul>

			<div class='terminal__footnote'>
				{section.footnote?.loaded && (
					<p>
						<span aria-hidden='true'>✓</span>{' '}
						{interpolate(section.footnote.loaded, {
							loaded: total,
							total,
							percent: 100,
						})}
					</p>
				)}
				{section.footnote?.renderTime && (
					<p>
						<span aria-hidden='true'>⚑</span> {section.footnote.renderTime}
					</p>
				)}
			</div>
		</div>
	);
}
