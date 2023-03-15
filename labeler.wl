(* ::Package:: *)

(* Labeler.wl *)
(* Copyright (C) 2023 Alec Graves *)
(* Package with utilities for building bounding box detection datasets. *)


(* ~~ General Utilities ~~ *)

(*find images*)
findImages[dir_] := Sort@FileNames[
	{"*.png", "*.jpg", "*.jpeg", "*.tiff", "*.jp2", "*.j2k"},
	dir, Infinity, IgnoreCase -> True];

(* return Rectangle[{x1, y1}, {x2, y2}] *)
getRectangles[points_] := Apply[Rectangle] /@ Partition[points, 2];

(* {"box", "box", "qr", "qr"} -> {"box", "qr"} *)
getLabels[labels_] := First /@ Partition[labels, 2];

(* return {Labeled[Rectangle[{x1, y1}, {x2, y2}], "label"], ...} *)
getHighlight[points_, labels_, imageSize_] := 
	If[Length@points < 2, {}, (*not enough points*)
		(*else, we have enough points for a box*)
		MapThread[Labeled, {
			getRectangles[Transpose[Transpose[points]*imageSize]],
			getLabels[labels]}]];

(* return boxes = label->{{x1, y1}, {x2, y2}} *)
toBoxes[points_, labels_] := ((#[[1]] -> Transpose[Sort /@ Transpose[#[[2]]]]) &
	/@ Transpose[{getLabels[labels], Partition[points, 2]}]);

(* return {points, labels} = {{x11, y11}, {x12, y12}, {x21, y21}, ...},
	{label1, label1, label2, label2, ...}} *)
fromBoxes[boxes_] := If[Length@boxes == 0, {{}, {}},
  (* Else, boxes exist. *)
	{
		Transpose[Transpose[Flatten[Values[boxes], 1]]],
		Flatten[Transpose[{Keys[boxes], Keys[boxes]}]]
	}];

(* Get json label file name from image path *)
extensionReplace[path_, new_] := 
	With[{ext = FileExtension[path]}, StringTrim[path, ext] <> new];
imageToLabel[path_] := extensionReplace[path, "json"];

(* Save boxes. Overwrite with empty if existing.
Do not save empty if no label file present. *)
save[imageName_, boxes_] := With[{json = imageToLabel[imageName]},
	If[FileExistsQ[json] || Length[boxes] > 0, 
		Export[json, boxes, "json"]]];

(* Load our json label file *)
load[imageName_] := With[{json = imageToLabel[imageName]}, 
	If[FileExistsQ[json], 
		Import[json, "json"],
		{}]];


(* ~~ Box Labeler GUI ~~ *)

(* Main labeling GUI function; pass in a directory and list of string labels *)
BoundingBoxLabeler[photoDirectory_, possibleLabels_] :=	DynamicModule[{
		pts = {}, (* (x1, y1), {x2, y2}, {x3, y3}, {x4, y4}, ... *)
		labels = {}, (* box, box, qr, qr, qr, qr, box, box, ... *)
		photoNames = findImages[photoDirectory],
		idx = 1, (* Image name index *)
		currentLabel = First[possibleLabels], (* Current bounding box class *)
		imageSize,
		undo, init, clear, prev, next
		},

	(* Empty points and labels arrays *)
	clear[] := (pts = {}; labels = {});

	(* Load boxes from a label file if one exists. *)
	init[] := ({pts, labels} = fromBoxes[load[photoNames[[idx]]]]);

	(* Delete the last box *)
	undo[] := If[Length[pts] >= 2, 
			pts = Flatten[Partition[pts, 2][[;; -2]], 1];
			labels = Flatten[Partition[labels, 2][[;; -2]]]
	];
	
	(*go to the previous image*)
	prev[] := (
			save[photoNames[[idx]], toBoxes[pts, labels]];
			idx = Mod[idx - 1, Length[photoNames], 1];
			init[]);

	(*go to the next image*)
	next[] := (
			save[photoNames[[idx]], toBoxes[pts, labels]];
			idx = Mod[idx + 1, Length[photoNames], 1];
			init[]);

	(* Set our variables *)
	init[];

	(* Outer EventHandler for keyboard actions *)
	EventHandler[

		If[Length[photoNames] == 0, None, (* Do nothing if we do not have images *)
		(* Otherwise, we found images. *)
		Column[{
				(* Basic UI buttons: *)
				Button["Clear", clear[], ImageSize -> Automatic],
				Button["Undo", undo[], ImageSize -> Automatic],
				Row[{Dynamic@SetterBar[Dynamic[currentLabel], possibleLabels],
						Button["Previous", prev[]],
						Button["Next", next[]]
						}],
						(* Draw pretty pictures / inner mouse EventHandler: *)
						Dynamic[EventHandler[
							HighlightImage[
									(* Store image dimensions *)
									With[{g = Graphics[
											ImageResize[Import[photoNames[[idx]]], {480}],
											ImageSize -> Large,
											Method -> {"ShrinkWrap" -> True},
											PlotRangePadding -> None]},
										imageSize = ImageDimensions[g];
										g],
									getHighlight[pts, labels, imageSize]],
							{
							(* On mouse down, store new box points *)
							"MouseDown" :> With[{p = MousePosition["Graphics"] },
								pts = AppendTo[pts, p/imageSize];
								labels = AppendTo[labels, currentLabel]]
							}],
							(* Improve performance, only track needed updates *)
							TrackedSymbols :> {idx, pts},
							SynchronousUpdating -> False],
				(* Display some final stats: *)
				Row[{Dynamic@idx, " of ", Dynamic@Length@photoNames}],
				Row[{"Bounding Boxes: ", Dynamic@Length@toBoxes[pts, labels]}]
			}]],
	
		(*EventHandler for keyboard bindings.*)
		(* Note, you need to have the output cell selected for these to work. *)
		(* Try clicking some the text if keyboard is not working. *)
		{
			"LeftArrowKeyDown" :> prev[],
			"RightArrowKeyDown" :> next[],
			{"KeyDown", "d"} :> next[],
			{"KeyDown", "a"} :> prev[],
			{"KeyDown", " "} :> next[],
			"EscapeKeyDown" :> undo[],
			{"KeyDown", "Backspace"} :> undo[],
			"KeyDown" :> (With[{k = Interpreter["Integer"]@CurrentValue["EventKey"]},
				If[k > 0 && k <= Length[possibleLabels], 
					currentLabel = possibleLabels[[k]]]])
		}
	]
];

(* SegmentationLabeler, only requires extensionReplace from above.  *)

masksize = 512

emptyImage[width_, height_] := ImageData[Binarize@Image[Table[0, {width}, {height}], ColorSpace -> "Grayscale"]]

(* img.png -> img.dog.bmp *)
bmpLabelName[imageName_, label_] := extensionReplace[imageName, label] <> ".bmp"

(* export mask *)
saveBmpLabel[imageName_, label_, mask_] := Export[bmpLabelName[imageName,label],
	ImageResize[Binarize @ Image[mask, ColorSpace -> "Grayscale"], {masksize}], "ImageCompression" -> "RLE", 
	"Channels"->1, "BitDepth"->1];

(* import mask *)
loadBmpLabel[imageName_, label_] := With[
	{name = bmpLabelName[imageName, label]}
	,
	If[FileExistsQ[name], 
		Binarize@Import[name,"Channels"->1, "BitDepth"->1]
		, (* else: *)
		emptyImage[masksize, masksize]
	]
];

(* GUI for semantic segmentation labels, exports a bitmap mask. *)
SegmentationLabeler[imageDirectory_, label_] := DynamicModule[
	{
		images = findImages[imageDirectory],
		idx = 1,
		state = "draw" (*or erase*),
		thickness = 0.05 (*width fraction*), 
		widths = {},
		colors = {},
		pts = {},
		mask = emptyImage[masksize, masksize],
		size = None
	},
	If[Length[images] == 0, None
	, (* else, images were found. *)

	(* Try to load the first mask *)
	mask = loadBmpLabel[images[[idx]], label];
	Column[{
		(* Basic UI buttons: *)
		Button["Clear", (
			mask = emptyImage[masksize, masksize];
			pts = {}; widths = {}; colors = {}
		), ImageSize -> Automatic],

		Row[{
			Dynamic @ SetterBar[Dynamic[state], {"draw", "erase"}],
			Button["Previous", (
				saveBmpLabel[images[[idx]], label, mask];
				idx = Mod[idx - 1, Length[images], 1];
				mask = loadBmpLabel[images[[idx]], label];)],
			Button["Next",(
					saveBmpLabel[images[[idx]], label, mask];
					idx = Mod[idx + 1, Length[images], 1];
					mask = loadBmpLabel[images[[idx]], label];
				)],
			"Brush Size: "
			(* Set the thickness of the brush stroke line *)
			Slider[Dynamic[thickness], {0.01, 0.1, 0.001}]
		}],

		(* Main UI Window for drawing *)
		Dynamic[Module[{img = ImageResize[Import[images[[idx]]], {480}]},
			size = ImageDimensions[Graphics[
				img, 
				ImageSize -> Medium, Method -> {"ShrinkWrap" -> True}, PlotRangePadding -> None]];
			EventHandler[
				Graphics[{
					{ImageResize[img, size]},
					{ Opacity[0.3],
						ImageResize[Image[mask, ColorSpace -> "Grayscale"], size], (* Mask *)
						Dynamic[Transpose @ {colors, Thickness /@ widths, Line /@ pts}] (* Active Brush line *)
					}},
					ImageSize -> size, 
					PlotRange -> {{0, size[[1]]}, {0, size[[2]]}},
					PlotRangePadding -> None
				],

				(* EventHandler Events: *)
				{
					(* On mouse down, store new points *)
					"MouseDown" :> (
						pts = AppendTo[pts, {MousePosition["Graphics"]}];
						widths = AppendTo[widths, thickness];
						colors = AppendTo[colors, If[state == "draw", White, Black]]),
					"MouseDragged" :> (
						pts[[-1]] = AppendTo[pts[[-1]], MousePosition["Graphics"]]),
					"MouseUp" :> (
						pts[[-1]] = AppendTo[pts[[-1]], MousePosition["Graphics"]];

						(* Bake the brush stroke back into the mask. *)
						mask = ImageData@Image[Graphics[{
								Opacity[1.0], ImageResize[Image[mask, ColorSpace -> "Grayscale"], size],
								Opacity[1.0], Transpose@{colors, Thickness /@ widths, Line /@ pts}},
							ImageSize -> size, 
							PlotRange -> {{0, size[[1]]}, {0, size[[2]]}},
							PlotRangePadding -> None], 
							ColorSpace -> "Grayscale"];

						(* Clear the paint. *)
						pts = {}; widths = {}; colors = {};
					)
				}]],

			(* Optimizations for Dynamic image. *)
			TrackedSymbols :> {idx, pts, mask},
			SynchronousUpdating -> False
		],

	(* Final output *)
	Row[{Dynamic@idx, " of ", Dynamic@Length@images}]
	}]]
]
