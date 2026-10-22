import { sectionData } from '../content';

export function Safari() {
	const section = sectionData.articles;
	if (!section) return null;

	return (
		<div>
			<div class='safari__toolbar'>
				<span class='flex items-center gap-2 text-label-3'>
					<span aria-hidden='true'>‹</span>
					<span aria-hidden='true'>›</span>
				</span>
				{section.url && (
					<div class='safari__search'>
						<span aria-hidden='true'>🔒</span>
						<span>{section.url}</span>
					</div>
				)}
				<span class='text-label-3' aria-hidden='true'>
					+
				</span>
			</div>

			<div class='safari__body'>
				{section.heading && (
					<h3
						class='mb-4 text-lg font-bold'
						style={section.headingColor ? { color: section.headingColor } : undefined}>
						{section.heading}
					</h3>
				)}
				<div>
					{section.posts.map((post) => (
						<article key={`${post.title}-${post.link}`} class='article'>
							{post.image && <img src={post.image} alt='' loading='lazy' />}
							<div>
								<p>{post.date}</p>
								<h3>{post.title}</h3>
								{section.cta && (
									<a href={post.link} target='_blank' rel='noopener noreferrer'>
										{section.cta}
									</a>
								)}
							</div>
						</article>
					))}
				</div>
			</div>
		</div>
	);
}
