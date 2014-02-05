import subprocess
import fileinput

if __name__ == '__main__':
	s = subprocess.check_output(['elm', '--make', '--runtime=elm-runtime.js', 'Main.elm'])
	print(s)

	if s.endswith('Generating HTML ... Done\r\n'):
		print('Inserting random seed port...')

		path = r'build\Main.html'

		with open(path, 'r') as f:
			data = f.read()

		new_data = data.replace('Elm.fullscreen(Elm.Main)',
			'Elm.fullscreen(Elm.Main, { seed: Math.floor(Math.random() * Math.pow(2, 32))})')

		with open(path, 'w') as f:
			f.write(new_data)
