/**
 * A GitHub Pages project site is served from `/<repo>/`, so the router, the links it renders and
 * the prerendered root all have to agree on that prefix. Vite bakes `base` in from `PAGES_BASE`.
 */
export const BASE = import.meta.env.BASE_URL.replace(/\/+$/, '');

/** The site root *as the router sees it* — preact-iso strips trailing slashes from `path`. */
export const HOME = BASE === '' ? '/' : BASE;
