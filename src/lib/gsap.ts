export type Gsap = typeof import('gsap').gsap;
export type DraggableCtor = typeof import('gsap/Draggable').Draggable;

let core: Promise<Gsap> | null = null;
let draggable: Promise<DraggableCtor> | null = null;

/**
 * Loads GSAP core lazily and on the client only, so it stays out of the initial
 * bundle and never evaluates during prerender.
 */
export function loadGsap(): Promise<Gsap> {
	if (typeof window === 'undefined') {
		return Promise.reject(new Error('GSAP is client-only'));
	}
	if (!core) {
		core = import('gsap').then((module) => module.gsap);
	}
	return core;
}

/** Loads and registers GSAP's Draggable plugin, once. */
export function loadDraggable(): Promise<DraggableCtor> {
	if (typeof window === 'undefined') {
		return Promise.reject(new Error('Draggable is client-only'));
	}
	if (!draggable) {
		draggable = Promise.all([loadGsap(), import('gsap/Draggable')]).then(
			([gsap, { Draggable }]) => {
				gsap.registerPlugin(Draggable);
				return Draggable;
			}
		);
	}
	return draggable;
}
