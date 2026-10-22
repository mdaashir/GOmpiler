import { useEffect, useRef } from 'preact/hooks';

import { profile } from '../content';
import { loadGsap } from '../lib/gsap';

function Letters({ text, className }: { text: string; className: string }) {
	return (
		<>
			{[...text].map((char, index) => (
				<span key={`${char}-${index}`} class={`hero-letter ${className}`} data-letter>
					{char === ' ' ? '\u00a0' : char}
				</span>
			))}
		</>
	);
}

export function Welcome() {
	const ref = useRef<HTMLElement>(null);

	useEffect(() => {
		const root = ref.current;
		if (!root) return;

		let tween: { kill: () => void } | null = null;
		let cancelled = false;

		loadGsap()
			.then((gsap) => {
				if (cancelled) return;
				const letters = root.querySelectorAll<HTMLElement>('[data-letter]');
				if (!letters.length) return;
				tween = gsap.fromTo(
					letters,
					{ fontVariationSettings: '"wght" 120', y: 6, autoAlpha: 0.5 },
					{
						fontVariationSettings: '"wght" 420',
						y: 0,
						autoAlpha: 1,
						duration: 1.1,
						ease: 'power2.out',
						stagger: { each: 0.03, from: 'start' },
						repeat: -1,
						repeatDelay: 2.4,
						yoyo: true,
					}
				);
			})
			.catch(() => undefined);

		return () => {
			cancelled = true;
			tween?.kill();
		};
	}, []);

	return (
		<section id='welcome' ref={ref} class='abs-center col-center px-6 text-center'>
			<p class='whitespace-nowrap text-3xl max-sm:text-2xl'>
				<Letters text={profile.greeting} className='font-sf' />
			</p>
			{/* Figma: the hero word is 100px Playwrite US Trad. */}
			<h1 class='mt-2 whitespace-nowrap text-[6.25rem] leading-[1.1] max-sm:text-7xl'>
				<Letters text={profile.headline} className='font-playwrite' />
			</h1>
		</section>
	);
}
