# Introduction
Everyday, I use Emacs as the universal tool to do most work, including
- editing all kinds of source codes (e.g., **Matlab**, **C/C++**, **Latex**, **Python**, **Html**, **Css**);
- simulating shells for running **Matlab**, **Python** and **Zsh**;
- connecting server through **SSH** and running multiple processes via **Screen**;
- organizing folders and documents on my Mac;
- taking notes (e.g., **Org**, **Markdown**) and synchronizing them with **Evernote**;
- maintaining **Git** repositories.

The reason I like Emacs is very simple: **being focused and
productive**. Most time I have only two softwares running, **Spotify**
for playing music and **Emacs** for working. Always staying in a
single Emacs environment can largely save your time in switching
between different softwares and reducing the possibility of being
distracted by other information (e.g., Facebook, Email, News,
Youtube). In addition, you can stick with the same group of key
shortcuts to improve your work-flows for different tasks.

<!-- As a software being developed for 30 years, -->
<!-- Emacs can offer your great tools to finish your work in very efficient way. -->
<!-- This page contains my Emacs configuration. -->

# Emacs Build Choice
I recommend to use the Emacs version from
[Railwaycat's Port](https://github.com/railwaycat/emacs-mac-port),
which provides a native GUI support for latest Mac OSX.

# Installation
1. Download and install [Prelude](https://github.com/bbatsov/prelude) as the default Emacs configuration;
2. Install the packages, `multiple-cursor`, `buffer-move`, and `auctex` via `M-x package-install`;
3. Copy `my-basic.el` and `matlab/` to the fold `~/.emacs.d/personal`;

# Matlab
I barely use the default Matlab user interface. Instead, I run Matlab in Emacs.
To start the Matlab shell in Emacs, you could type `C-m l` using my Emacs configuration.
