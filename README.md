#Installation Instructions
##1. Download

	git clone https://github.com/Raviyanto/Emacs_Config.git

##2. Install Font
	 
	 $ cd Emacs_Config
	 $ cp -R bitstream_vera_mono ~/.local/share/fonts
	 $ cd $HOME
	 $ fc-cache -f -v
	
##3. Install Emacs Configuration

	$ cd Emacs_Config
	$ cp moe-theme.el vendor ~/.emacs.d
	$ cp .emacs $HOME

##4. Image Appearance 
![Image Startup](https://github.com/Raviyanto/Emacs_Config/blob/master/screenshot_emacs.png  "Startup Emacs")
