import { Window } from './Window';
import { doc, getSection, hasDoc, hasSection, icon, locations } from '../content';
import { useWindows } from '../lib/window-context';
import { Contact } from '../windows/Contact';
import { Docs } from '../windows/Docs';
import { Finder } from '../windows/Finder';
import { ImgFile } from '../windows/ImgFile';
import { Photos } from '../windows/Photos';
import { Resume } from '../windows/Resume';
import { Safari } from '../windows/Safari';
import { Terminal } from '../windows/Terminal';
import { TxtFile } from '../windows/TxtFile';

export function WindowManager() {
	const { windows } = useWindows();
	const txt = windows.txtfile.data?.file;
	const img = windows.imgfile.data?.file;

	// The finder is worth mounting as soon as any location exists; the file viewers exist to show
	// the files it opens.
	const showFinder = Object.keys(locations).length > 0;

	return (
		<>
			{showFinder && (
				<>
					{/* The finder carries a toolbar icon instead of a centred title. */}
					<Window
						id='finder'
						title={getSection('projects')?.title ?? 'Finder'}
						className='finder'
						showTitle={false}
						headerRight={
							<img src={icon('menubar.search')} alt='' class='h-5 w-5 opacity-70 dark:invert' />
						}>
						<Finder />
					</Window>
					<Window id='txtfile' title={txt?.name ?? 'Untitled.txt'} className='txtfile'>
						<TxtFile />
					</Window>
					<Window id='imgfile' title={img?.name ?? 'Untitled.png'} className='imgfile'>
						<ImgFile />
					</Window>
				</>
			)}

			{hasSection('articles') && (
				<Window id='safari' title={getSection('articles')?.title ?? 'Articles'} className='safari'>
					<Safari />
				</Window>
			)}

			{hasSection('gallery') && (
				<Window id='photos' title={getSection('gallery')?.title ?? 'Gallery'} className='photos'>
					<Photos />
				</Window>
			)}

			{hasSection('contact') && (
				<Window id='contact' title={getSection('contact')?.title ?? 'Contact'} className='contact'>
					<Contact />
				</Window>
			)}

			{hasSection('skills') && (
				<Window id='terminal' title={getSection('skills')?.title ?? 'Skills'} className='terminal'>
					<Terminal />
				</Window>
			)}

			{hasSection('resume') && (
				<Window id='resume' title={getSection('resume')?.title ?? 'Resume'} className='resume'>
					<Resume />
				</Window>
			)}

			{/* A documentation site is nothing but this reader, so it opens straight away. */}
			{hasDoc && (
				<Window id='docs' title={doc.title} className='docs'>
					<Docs />
				</Window>
			)}
		</>
	);
}
