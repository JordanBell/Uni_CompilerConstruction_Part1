#!/bin/bash
echo "Adding all known files..."
git add src
git add test_cases
git add gitpush.sh
git add build.sh
git add setup_opam.sh
git add setup_atom.sh
git add setup_git.sh
git add test.sh
git add README.md
echo "...complete."
echo "Git status:"
git status

#Do the commit, with permission
read -p "Do you wish to commit? " yn
case $yn in
	[Yy]* ) echo "Please enter commit details: "
		read commit_deets
		git commit -m "$commit_deets";;
	[Nn]* ) exit;;
esac

#Do the push, with permission
read -p "Do you wish to push? " yn
case $yn in
	[Yy]* ) git push -u origin master;;
	[Nn]* ) exit;;
esac
