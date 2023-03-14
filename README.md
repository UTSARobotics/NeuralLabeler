# R5 NeuralLabeler 🧠

The intelligent way to label data... I think...

## Getting Started

This project uses Mathematica notebooks for a GUI component.

Mathematica is nice because it is cross-platform and has a massive standard library for many tasks related to robotics from image processing and machine learning to symbolic math to GUI design. **It is also free for UTSA students.**

Mathematica Setup Steps:

1. To get free access to Mathematica, go to the [Access to Wolfram Products](https://www.wolfram.com/siteinfo/) page and enter your UTSA email.

2. Next, go to the [Wolfram Account](https://account.wolfram.com/) page and sign in with your UTSA email.

3. Then, go to the [User Portal](https://user.wolfram.com/portal/myProducts.html) page and select "Get Downloads" next to the latest version of Mathematica you have access to.

4. Select the download for your operating system. **"Mathematica (with web documentation access)"** is preferred because it is a smaller download.

5. When finished, enter the activation key from the same downloads page.

## NeuralLabeler usage 🖼️ 

Make sure you have downloaded the dataset you want to label to your computer. The dataset is nothing more than a folder of pictures. Next, open the `NeuralLabeler.nb` Mathematica notebook. 

1. Run the initialization cell to load in the labeler functions
2. Run the dataset folder cell and click on the "Browse..." icon to select the image folder you want to label.
3. Set the list of labels to use {"label1","label2", ...}
4. Run the `labeler[photoDirectory, possibleLabels]` function to start the labeling GUI.

Labeling: ✏️

* To draw a box, click on one corner then click on the other.
* Labels are saved automatically when you switch images. 
* You can start by using the buttons at the top of the labeling GUI.
* You can also use keyboard shortcuts
  * Number keys select the current label
  * Space bar is the next image
  * Right/Left arrows are next/previous image
  * "a" and "d" are also next/previous
  * Esc or Backspace deletes the last box.

*Note, if the keyboard shortcut keys do not work, you need to select the cell that has the GUI. The easiest way to do this is to click on the text at the bottom of the GUI like "Bounding Boxes" next to the box count.*

## Additional Utilities 🛠️

Our notebook also has utilities for working with and collecting image data. The first tool "Image Optimizer" will resize images and save them as .png formatted data. You can change the size using the "imageSize" variable.

There is also a "VideoSplitter" utility that will read a video file and save images at the specified frame rate to an output folder (by default next to the video file selected). Use this to generate training data from a video file.

To run these utilities, execute them one block at a time using `[shift] + [enter]` and select input folders/file paths as needed.

## Manifest

* `labeler.wl` - a collection of WL (Wolfram Language) functions to help with bounding box labels
* `NeuralLabeler.nb` - a Mathematica notebook for creating bounding box labels
* `README.md` - your friendly guide into the world of symbolic computation for data processing
* `COPYING` - ⚖️ the MIT license ⚖️