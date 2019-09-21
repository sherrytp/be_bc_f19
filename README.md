# be_bc_f19

* This repository contains the course materials for Behavioral Economics at Boston College, Fall 2019
* Assignments are in the [GitHub Classroom](https://classroom.github.com/classrooms/54641384-be_bc_f19)


## Git(Hub)

You can clone this repository to your computer and pull updates when they are posted. You should keep your personal course files (notes, code files with your comments, etc.) in a separate repository. 

First make sure you installed Git: https://git-scm.com/book/en/v2/Getting-Started-Installing-Git

To get class files on your Desktop:

1. Open your Terminal and navigate to where you want keep class files
	- e.g. if I want to put it on my desktop I run `cd ~/Desktop` on the Terminal command line
2. Now clone this repository: `git clone https://github.com/behavioral-economics/be_bc_f19.git`
3. To download and integrate changes to the repository run: `git pull`, short for `git pull origin master`. 
	* Note: you can also use `git fetch`, which downloads but does not integrate. [Big debate](https://stackoverflow.com/questions/292357/what-is-the-difference-between-git-pull-and-git-fetch) about which is better. Since the class repository contains material that only I edit, I recommend using `git pull`. 

To set up and manage your own repository:

1. Create a repository on GitHub and give it a good name (e.g. "be_bc_f19_yourname")
2. Clone it to your computer
3. All files in this repository are now tracked by Git. When you are ready to "save" a file with Git, run `git add filename` (or to save everything run `git add -A`)
4. Commit your saves with a message: `git commit -m "changed the filename"`. Make sure the message is meaningful. 
5. Now push the saves to GitHub, the remote repository: `git push`

You can check the status of a repository at any time with `git status`.

Some helpful resources:

* The fastest guide to Git(Hub) I have ever seen: https://rogerdudler.github.io/git-guide/
* A more detailed treatment: https://swcarpentry.github.io/git-novice/