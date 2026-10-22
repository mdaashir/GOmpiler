# Content

Everything the site displays comes from this folder. Edit these files, run `npm run build`, and the
site updates — no component code needs to change.

## Files

| File | What it controls |
| --- | --- |
| `site.json` | Site name, brand, domain, SEO title/description, theme colour, default appearance |
| `doc.json` | Docs window title, subtitle, repo link, sidebar heading and the **sections** list |
| `docs/*.md` | The documentation itself — one Markdown file per section |
| `profile.json` | The wallpaper hero: the greeting line and the script headline |
| `menu.json` | The menu bar's links and status icons; the dock list |
| `desktop.json` | Desktop context-menu items and the file shown by **Get Info** |
| `icons.json` | Every icon by role — `apps.*`, `files.*`, `menubar.*`, `sidebar.*`, `social.*` |
| `phone.json` | The phone home screen shown below 640px |

`content/sections/` is intentionally empty in a docs site: there are no portfolio apps (Finder,
Photos, Mail…) to drive, so the docs window is the only window rendered.

## Adding a page

Drop a Markdown file in `docs/` and add a section to `doc.json`:

```json
{ "id": "deploy", "title": "Deploying", "file": "deploy.md", "icon": "sidebar.file" }
```

The `id` becomes the hash the reader writes as you navigate — `#/deploy`. A section whose Markdown
file is missing is dropped automatically, so a typo leaves no empty sidebar entry.

## The dock and the menu bar

A docs site has no portfolio apps, so both are filled from the docs sections instead: the dock shows
one icon per section (artwork mapped in `src/components/Dock.tsx`) and the menu bar lists the section
titles. Clicking either opens the docs window at that section. Order follows `doc.json`.

## Icons

`icons.json` maps a role to either a bundled icon name (`wifi`, `folder`, `github`) or a path under
`public/`. Paths are written as `/images/…` and get the Pages base path prefixed at build time, so
they resolve on a project site as well as on a root site.

The tests check that every referenced path exists, so a typo fails the build instead of silently
rendering a broken image.

## Hero

`profile.json` drives the text in the wallpaper:

```json
{ "greeting": "Write C++ or Go,", "headline": "ship native." }
```

`greeting` renders in the system sans face, `headline` in Playwrite US Trad — the same pairing the
portfolio uses. The docs window covers it while open, exactly as the portfolio's windows do.
