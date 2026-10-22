import { site } from '../content';
import { useWindows } from '../lib/window-context';

export function ImgFile() {
	const file = useWindows().windows.imgfile.data?.file;

	return (
		<div class='imgfile__body'>
			{file?.imageUrl ? (
				<img src={file.imageUrl} alt={file.name} />
			) : (
				<p class='p-6 text-sm text-label-2'>{site.ui.noImageSelected}</p>
			)}
		</div>
	);
}
