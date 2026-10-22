# GOmpiler docs — the site at mdaashir.github.io/GOmpiler

Documentation for the two toolchains in this repository, published in the same macOS window theme
as [mdaashir.github.io](https://mdaashir.github.io/). The branch contains **only the site**; the
compilers themselves live on `master` (GOmpiler) and `go2cpp`.

- **Live:** https://mdaashir.github.io/GOmpiler/
- **Stack:** Vite + Preact + Tailwind, prerendered to static HTML
- **Deploy:** every push to this branch runs `.github/workflows/deploy.yml` — `npm ci` → `npm test`
  → `npm run build` (with `PAGES_BASE=/<repo>/`) → GitHub Pages. No manual step.

## Editing the documentation

Everything readable comes from `content/` — no component changes needed.

| File | Controls |
| --- | --- |
| `content/site.json` | Project name, browser title, description, default appearance |
| `content/doc.json` | Window title, subtitle, repo link, sidebar heading and the section list |
| `content/docs/*.md` | One Markdown file per sidebar section |
| `content/profile.json` | The wallpaper hero: greeting line and script headline |
| `content/menu.json` | Menu-bar links and status icons |
| `content/icons.json` | Icon roles — app, file, menu-bar, social |
| `content/desktop.json` | Desktop context menu and Get Info |

Add a page by dropping a Markdown file in `content/docs/` and adding a section to `doc.json`:

```json
{ "id": "deploy", "title": "Deploying", "file": "deploy.md", "icon": "sidebar.file" }
```

The `id` is the deep link: `https://mdaashir.github.io/GOmpiler/#/deploy`.

## Local development

```bash
npm install --include=dev
npm run dev        # http://localhost:5173
npm test
PAGES_BASE=/GOmpiler/ npm run build
```

`PAGES_BASE` is empty in development and set to `/<repo>/` in CI, so asset URLs resolve both on a
project Pages site and on the root site.

## Notes

- The docs window opens immediately; the dock and the menu bar carry the same sections, so the
  desktop matches the portfolio rather than showing an empty shelf. The selected section is written
  to the URL as `#/<id>` as you navigate.
- This branch is derived from the `template` branch of
  [mdaashir.github.io](https://github.com/mdaashir/mdaashir.github.io); when that theme changes,
  cherry-pick the change rather than re-copying over the content.
