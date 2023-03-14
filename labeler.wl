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
labeler[photoDirectory_, possibleLabels_] :=	DynamicModule[{
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
