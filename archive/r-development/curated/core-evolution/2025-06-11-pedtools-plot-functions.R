# Archived R development file
# Original path: archive version/full_functions.R
# Original created: 2025-06-11 15:24:28
# Original modified: 2025-06-11 15:24:28
# Archive rationale: Custom plotting and annotation helpers derived from pedigree rendering experiments.
# Active app status: not sourced by the current application; kept for project history and technical review.
# -----------------------------------------------------------------------------



# Alignment ---------------------------------------------------------------

#' @rdname plotmethods
#' @importFrom kinship2 align.pedigree
#' @export
.pedAlignment = function(x = NULL, plist = NULL, arrows = FALSE, twins = NULL,
                         miscarriage = NULL, packed = TRUE, width = 10,
                         straight = FALSE, align = NULL,
                         spouseOrder = NULL, hints = NULL, ...) {
  
  if(hasSelfing(x) && !arrows) {
    message("Pedigree has selfing, switching to DAG mode. Use `arrows = TRUE` to avoid this message.")
    arrows = TRUE
  }
  
  if(!is.null(plist))
    return(.extendPlist(x, plist, arrows = arrows, miscarriage = miscarriage))
  
  # Singleton
  if(is.singleton(x)) {
    plist = list(n = 1, nid = cbind(1), pos = cbind(0), fam = cbind(0), spouse = cbind(0))
    return(.extendPlist(x, plist, miscarriage = miscarriage))
  }
  
  if(arrows)
    return(.alignDAG(x))
  
  # Twin data: enforce data frame
  if(is.vector(twins))
    twins = data.frame(id1 = twins[1], id2 = twins[2], code = as.integer(twins[3]))
  
  # (Try to) force spouse order
  if(!is.null(spouseOrder)) {
    if(!is.null(hints))
      stop2("Cannot use both `hints` and `spouseOrder` in the same call")
    hints = .spouseOrder(x, spouseOrder)
  }
  
  k2ped = as_kinship2_pedigree(x, twins = twins)
  align = align %||% if(straight) c(0,0) else c(1.5, 2)
  plist = kinship2::align.pedigree(k2ped, packed = packed, width = width, align = align, hints = hints)
  
  # Catch missing persons (kindepth bug!)
  ERR = sum(plist$n) < length(x$ID)
  if(ERR) {
    warning("Alignment failed; switching to simple DAG alignment", call. = FALSE)
    return(.alignDAG(x))
  }
  
  # Ad hoc fix for 3/4 siblings and similar
  if(is.null(hints))
    plist = .fix34(x, k2ped = k2ped, plist = plist, packed = packed, width = width, align = align)
  
  # Fix annoying rounding errors in first column of `pos`
  plist$pos[] = round(plist$pos[], 6)
  
  # Add further parameters
  .extendPlist(x, plist, miscarriage = miscarriage)
}


.extendPlist = function(x, plist, arrows = FALSE, miscarriage = NULL) {
  nid = plist$nid
  pos = plist$pos
  
  nInd = max(nid)
  maxlev = nrow(pos)
  
  id = as.vector(nid)
  plotord = id[id > 0]
  
  # Coordinates (top center)
  xall = pos[id > 0]
  yall = row(pos)[id > 0]
  
  xrange = range(xall)
  yrange = range(yall)
  
  # For completeness: Kinship2 order (1st instance of each only!)
  # Including this for completeness
  tmp = match(1:nInd, nid)
  xpos = pos[tmp]
  ypos = row(pos)[tmp]
  
  sex = getSex(x)
  if(length(miscarriage)) {
    idx = match(miscarriage, x$ID, nomatch = 0L)
    idx = idx[idx > 0]
    if(any(idx %in% c(x$FID, x$MID)))
      stop2("A parent cannot assigned as a miscarriage: ", miscarriage[idx %in% c(x$FID, x$MID)])
    sex[idx] = 3
  }
  
  list(plist = plist, x = xpos, y = ypos, nInd = nInd, sex = sex, ped = x, arrows = arrows,
       plotord = plotord, xall = xall, yall = yall, maxlev = maxlev, xrange = xrange, yrange = yrange)
}


# Annotation --------------------------------------------------------------

#' @rdname plotmethods
#' @export
.pedAnnotation = function(x, title = NULL, marker = NULL, sep = "/", missing = "-", showEmpty = FALSE,
                          labs = labels(x), foldLabs = 12, trimLabs = TRUE, col = 1, fill = NA, lty = 1, lwd = 1,
                          hatched = NULL, hatchDensity = 25, aff = NULL, carrier = NULL,
                          deceased = NULL, starred = NULL, proband = NULL, textAnnot = NULL,
                          textInside = NULL, textAbove = NULL, fouInb = "autosomal", ...){
  
  res = list()
  nInd = pedsize(x)
  
  
  # Title -------------------------------------------------------------------
  if(is.function(title))
    title = title(x)
  res$title = title
  
  
  # Labels ------------------------------------------------------------------
  
  if(is.function(labs))
    labs = labs(x)
  
  if(identical(labs, "num"))
    labs = setNames(x$ID, 1:nInd)
  
  textu = .prepLabs(x, labs)
  
  # Fold
  if(isCount(foldLabs))
    textu = vapply(textu, function(s) smartfold(s, width = foldLabs), FUN.VALUE = "")
  else if(is.function(foldLabs))
    textu = vapply(textu, foldLabs, FUN.VALUE = "")
  
  # Add stars to labels
  if(is.function(starred))
    starred = starred(x)
  starred = internalID(x, starred, errorIfUnknown = FALSE)
  starred = starred[!is.na(starred)]
  textu[starred] = paste0(textu[starred], "*")
  
  # Marker genotypes
  if (length(marker) > 0) { # excludes NULL and empty vectors/lists
    if (is.marker(marker))
      mlist = list(marker)
    else if (is.markerList(marker))
      mlist = marker
    else if (is.numeric(marker) || is.character(marker) || is.logical(marker))
      mlist = getMarkers(x, markers = marker)
    else
      stop2("Argument `marker` must be either:\n",
            "  * a n object of class `marker`\n",
            "  * a list of `marker` objects\n",
            "  * a character vector (names of attached markers)\n",
            "  * an integer vector (indices of attached markers)",
            "  * a logical vector of length `nMarkers(x)`")
    checkConsistency(x, mlist)
    
    gg = do.call(cbind, lapply(mlist, format, sep = sep, missing = missing))
    geno = apply(gg, 1, paste, collapse = "\n")
    
    if(is.logical(showEmpty) && length(showEmpty) == 1)
      showEmpty = if(showEmpty) x$ID else NULL
    else if (is.function(showEmpty))
      showEmpty = showEmpty(x)
    
    hideEmpty = match(x$ID, showEmpty, nomatch = 0L) == 0
    if (any(hideEmpty)) {
      isEmpty = rowSums(do.call(cbind, mlist)) == 0
      geno[isEmpty & hideEmpty] = ""
    }
    
    textu = if (!any(nzchar(textu))) geno else paste(textu, geno, sep = "\n")
  }
  
  if(trimLabs)
    textu = trimws(textu, which = "both", whitespace = "[\t\r\n]")
  
  res$textUnder = textu
  
  # Further text annotation
  
  if(!is.null(textAnnot)) {
    res$textAnnot = lapply(textAnnot, function(b) {
      if(is.atomic(b))
        b = list(b)
      b[[1]] = .prepLabs2(x, b[[1]])
      b
    })
  }
  
  
  # Text above symbols ------------------------------------------------------
  
  showFouInb = !is.null(fouInb) && hasInbredFounders(x)
  
  if(is.function(textAbove))
    textAbove = textAbove(x)
  else if(showFouInb) {
    finb = founderInbreeding(x, chromType = fouInb, named = TRUE)
    finb = finb[finb > 0]
    textAbove = sprintf("f = %.4g", finb)
    names(textAbove) = names(finb)
  }
  
  res$textAbove = .prepLabs2(x, textAbove)
  
  # Text inside symbols ------------------------------------------------------
  
  if(is.function(textInside))
    textInside = textInside(x)
  
  res$textInside = .prepLabs2(x, textInside)
  
  
  # Affected/hathced --------------------------------------------------------
  
  if(is.function(aff))
    aff = aff(x)
  if(is.function(hatched))
    hatched = hatched(x)
  isaff = x$ID %in% aff
  ishatch = x$ID %in% hatched
  
  # filling density (-1 = fill; 25 = hatch)
  densvec = integer(nInd)
  densvec[isaff] = -1
  densvec[ishatch] = hatchDensity
  res$densvec = densvec
  
  # See fill color below!
  
  # Colours (border)----------------------------------------------------------
  
  res$colvec = .prepPlotarg(x, col, default = 1)
  
  # Fill color --------------------------------------------------------------
  
  affORhatch = isaff | ishatch
  
  # If aff/hatch given apply simple fill only to those
  if(any(affORhatch) && !is.list(fill) && is.null(names(fill)) && !identical(fill, NA)) {
    fillvec = rep(NA, length = nInd)
    fillvec[affORhatch] = fill
  }
  else
    fillvec = .prepPlotarg(x, fill, default = NA)
  
  # Ensure aff/hatch are filled
  fillvec[affORhatch & is.na(fillvec)] = 1
  
  res$fillvec = fillvec
  
  
  # Linetype ----------------------------------------------------------------
  ltyvec = .prepPlotarg(x, lty, default = 1)
  
  if(any(badlty <- !ltyvec %in% 0:6)) {
    ltynames = c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
    ltyvec[badlty] = match(ltyvec[badlty], ltynames, nomatch = 2) - 1
  }
  res$ltyvec = as.numeric(ltyvec)
  
  # Line width ----------------------------------------------------------------
  
  res$lwdvec = as.numeric(.prepPlotarg(x, lwd, default = 1))
  
  # Carriers ----------------------------------------------------------------
  
  if(is.function(carrier))
    carrier = carrier(x)
  
  # Convert to T/F
  res$carrierTF = x$ID %in% carrier
  
  # Deceased ----------------------------------------------------------------
  
  if(is.function(deceased))
    deceased = deceased(x)
  
  # Convert to T/F
  res$deceasedTF = x$ID %in% deceased
  
  
  # Proband -----------------------------------------------------------------
  
  if(is.function(proband))
    proband = proband(x)
  
  # Convert to T/F
  res$probandTF = x$ID %in% proband
  
  # Return list -------------------------------------------------------------
  res
}


# Convert `labs` to full vector with "" for unlabels indivs
.prepLabs = function(x, labs) {
  id = rep("", length(x$ID)) # Initialise
  
  mtch = match(x$ID, labs, nomatch = 0L)
  showIdx = mtch > 0
  showLabs = labs[mtch]
  
  # Use names(labs) if present
  if(!is.null(nms <- names(labs))) {
    newnames = nms[mtch]
    goodIdx = newnames != "" & !is.na(newnames)
    showLabs[goodIdx] = newnames[goodIdx]
  }
  
  id[showIdx] = showLabs
  id
}

# Alternative to .prepLabs (used above/inside): If vector has names, match these to x$ID.
.prepLabs2 = function(x, labs) {
  mode(labs) = "character"
  
  if(is.null(names(labs)) && length(labs) == length(x$ID))
    return(labs)
  
  ids = names(labs) %||% labs
  mtch = match(x$ID, ids, nomatch = 0L)
  
  txt = rep("", length(x$ID)) # Initialise
  txt[mtch > 0] = labs[mtch]
  txt
}


#--- Plot dimension and scaling parameters

#' @rdname plotmethods
#' @importFrom graphics frame strheight strwidth
#' @export
.pedScaling = function(alignment, annotation, cex = 1, symbolsize = 1, margins = 1,
                       addSpace = 0, xlim = NULL, ylim = NULL, vsep2 = FALSE,
                       autoScale = FALSE, minsize = 0.15, debug = FALSE, ...) {
  
  textUnder = annotation$textUnder
  textAbove = annotation$textAbove
  title = annotation$title
  
  maxlev = alignment$maxlev
  xrange = alignment$xrange
  yrange = alignment$yrange
  nid = alignment$plist$nid
  nid1 = nid[1, ][nid[1, ] > 0] # ids in first generation
  
  # Fix xrange/yrange for singletons and selfings
  if(maxlev == 1 || xrange[1] == xrange[2])
    xrange = xrange + c(-0.5, 0.5)
  
  if(maxlev == 1)
    yrange = yrange + c(-0.5, 0.5)
  
  # Margins
  mar = margins
  if(length(mar) == 1)
    mar = if(!is.null(title)) c(mar, mar, mar + 2.1, mar) else rep(mar, 4)
  
  # Adjust margin for proband arrows?
  if(any(prb <- annotation$probandTF)) {
    idx = prb[alignment$plotord]
    # Hard coded: All arrows are bottom left
    arrowL = any(alignment$xall[idx] == xrange[1])
    arrowB = any(alignment$yall[idx] == yrange[2])
    
    extraMar = c(if(arrowB) 0.5 else 0, if(arrowL) 2.5 else 0, 0, 0)
    mar = mar + extraMar
  }
  
  # Extra padding (e.g. for ribd::ibdDraw() and ibdsim2::haploDraw())
  if(length(addSpace) == 1)
    addSpace = rep(addSpace, 4)
  
  # Set margin and xpd
  oldpar = par(mar = mar, xpd = TRUE)
  
  # Dimensions in inches
  psize = par('pin')
  
  # Shortcut for finding height of a string in inches. Empty -> 0!
  hinch = function(v) {
    res = strheight(v, units = "inches", cex = cex)
    res[v == ""] = 0
    res
  }
  
  # Text height
  labh_in = hinch('M') # same as for "1g" used in kinship2 (`stemp2`)
  
  # Make room for curved duplication lines involving first generation
  # A bit hackish since curve height is only available in user coordinates.
  curvAdj = if(anyDuplicated.default(nid1)) 0.5 else if(maxlev > 1 && any(nid1 %in% nid[2, ])) 0.1225 else 0
  
  # Text above symbols in first generation
  # Don't adjust for text above if also for curve. (NB: Fails in extreme cases)
  abovetop_in = if(curvAdj>0) 0 else max(hinch(textAbove[nid1]))
  
  # Add offset: "0.5 times the width [!] of a character"
  if(abovetop_in > 0)
    abovetop_in = abovetop_in + strwidth("W", units = "inches")/2
  
  abovetop_in = abovetop_in + addSpace[3]
  
  # Separation above/below labels
  labsep1_in = 0.7*labh_in  # above text
  labsep2_in = labh_in  # below text
  labsep3_in = 0.3*labh_in  # below text in last generation
  
  # Max label height (except last generation)
  maxlabh_nolast_in = if(maxlev == 1) 0 else max(hinch(textUnder[nid[seq_len(maxlev - 1), ]]))
  
  # Max label height in last generation
  belowlast_in = max(hinch(textUnder[nid[maxlev, ]]))
  
  # Everything below bottom symbol (label + space above and below)
  if(belowlast_in > 0)
    belowlast_in = belowlast_in + labsep1_in + labsep3_in
  
  belowlast_in = belowlast_in + addSpace[1]
  
  # KEY TO LAYOUT: Complete y-range (psize[2]) in inches corresponds to:
  # abovetop_in + (labsep1_in + h + maxlabh_nolast_in + labsep2_in) * (maxlev - 1) + (h + belowlast_in)
  
  # Symbol height restriction 1 (Solve above for h)
  if(maxlev > 1) {
    sep1_in = (labsep1_in + maxlabh_nolast_in + labsep2_in)
    ht1 = (psize[2] - abovetop_in - belowlast_in - sep1_in * (maxlev - 1)) / maxlev
  }
  else
    ht1 = psize[2] - abovetop_in - belowlast_in
  
  # Height restriction 2
  ht2 = psize[2]/(maxlev + (maxlev-1)/2)
  
  # Width restriction 1: Default width = 2.5 letters (`stemp1`)
  wd1 = strwidth("ABC", units='inches', cex=cex) * 2.5/3
  
  # Width restriction 2
  wd2 = psize[1] * 0.8/(.8 + diff(xrange))  # = psize[1] for singletons/selfings
  
  # Box size in inches
  boxsize = symbolsize * min(ht1, ht2, wd1, wd2)
  
  # Autoscale if too small
  if(autoScale && boxsize < minsize) {
    if(minsize > ht2 | cex < 0.2)
      stop2("autoScale error: `minsize` is too large for the current window")
    
    trycex = round(0.95 * cex, 2)
    trysymbolsize = round(1.05 * symbolsize, 2)
    if(debug)
      message(sprintf("autoScale: cex = %g, symbolsize = %g", trycex, trysymbolsize))
    
    return(.pedScaling(alignment, annotation, cex = trycex, symbolsize = trysymbolsize,
                       margins = margins, addSpace = addSpace, xlim = xlim, ylim = ylim,
                       vsep2 = vsep2, autoScale = TRUE, minsize = minsize, debug = debug))
  }
  
  if (ht1 <= 0)
    stop2("Labels leave no room for the graph, reduce cex")
  
  # Horizontal scaling factor
  if(is.null(xlim)) {
    # Segment corresponding to 1 unit
    hscale = (psize[1] - boxsize - addSpace[2] - addSpace[4])/diff(xrange)
  }
  else { # override if xlim provided!
    hscale = psize[1]/diff(xlim)
  }
  
  # Vertical scaling factor
  if(is.null(ylim)) {
    denom = if(maxlev == 1) 1 else maxlev - 1 + curvAdj
    vscale = (psize[2] - (abovetop_in + boxsize + belowlast_in)) / denom
  }
  else {
    vscale = psize[2]/diff(ylim)
  }
  
  if(hscale <= 0 || vscale <= 0)
    stop2("Cannot fit the graph; please increase plot region or reduce cex and/or symbolsize")
  
  boxw = boxsize/hscale  # box width in user units
  boxh = boxsize/vscale  # box height in user units
  
  # User coordinates
  if(is.null(xlim)) {
    left   = xrange[1] - 0.5*boxw - addSpace[2]/hscale
    right  = xrange[2] + 0.5*boxw + addSpace[4]/hscale
  }
  else {
    left = xlim[1]
    right = xlim[2]
  }
  if(is.null(ylim)) {
    top    = yrange[1] - abovetop_in/vscale - curvAdj
    bottom = yrange[2] + (boxsize + belowlast_in)/vscale
  }
  else {
    top = min(ylim)
    bottom = max(ylim)
  }
  usr = c(left, right, bottom, top)
  
  labh = labh_in/vscale        # height of a text string
  legh = min(1/4, boxh * 1.5)  # how tall are the 'legs' up from a child
  
  # Return plotting/scaling parameters
  list(boxw = boxw, boxh = boxh, labh = labh, legh = legh, vsep2 = vsep2,
       cex = cex, mar = mar, usr = usr, oldpar = oldpar)
}


#' @rdname plotmethods
#' @importFrom graphics lines polygon segments
#' @export
.drawPed = function(alignment, annotation, scaling) {
  
  if(isTRUE(alignment$arrows))
    return(.plotDAG(alignment, annotation, scaling))
  
  n = alignment$nInd
  plotord = alignment$plotord
  xall = alignment$xall
  yall = alignment$yall
  maxlev = alignment$maxlev
  plist = alignment$plist
  SEX = alignment$sex
  
  pos = plist$pos
  nid = plist$nid
  
  boxh = scaling$boxh
  boxw = scaling$boxw
  legh = scaling$legh
  vsep2 = scaling$vsep2
  
  branch = 0.6
  pconnect = .5
  
  COL = annotation$colvec %||% 1
  FILL = annotation$fillvec %||% NA
  LTY = annotation$ltyvec %||% 1
  DENS = annotation$densvec %||% 0
  LWD = annotation$lwdvec %||% 1
  
  if (length(COL) == 1)
    COL = rep(COL, n)
  if (length(FILL) == 1)
    FILL = rep(FILL, n)
  if (length(LTY) == 1)
    LTY = rep(LTY, n)
  if (length(DENS) == 1)
    DENS = rep(DENS, n)
  if (length(LWD) == 1)
    LWD = rep(LWD, n)
  
  # Set user coordinates
  par(mar = scaling$mar, usr = scaling$usr, xpd = TRUE)
  
  # Shapes
  POLYS = list(list(x = c(0, -0.5, 0, 0.5), y = c(0, 0.5, 1, 0.5)), # diamond
               list(x = c(-1, -1, 1, 1)/2, y = c(0, 1, 1, 0)),      # square
               list(x = 0.5 * cos(seq(0, 2 * pi, length = 50)),     # circle
                    y = 0.5 * sin(seq(0, 2 * pi, length = 50)) + 0.5),
               list(x = c(0, -1.2, 1.2)/2, y = c(0, 0.6, 0.6)))     # triangle
  
  # Draw all the symbols
  for (k in seq_along(plotord)) {
    id = plotord[k]
    poly = POLYS[[SEX[id] + 1]]
    dens = if(DENS[id] == 0) NULL else DENS[id]
    polygon(xall[k] + poly$x * boxw,
            yall[k] + poly$y * boxh,
            border = COL[id], col = FILL[id],
            lty = LTY[id], lwd = LWD[id],
            angle = 45, density = dens)
  }
  
  ## Add lines between spouses (MDV: Vectorized/simplified)
  sp = plist$spouse
  cl = col(sp)[sp > 0]
  rw = row(sp)[sp > 0]
  tmpy = rw + boxh/2
  segments(pos[cbind(rw, cl)]     + boxw/2, tmpy,
           pos[cbind(rw, cl + 1)] - boxw/2, tmpy)
  
  # Double line for consanguineous marriage
  if(any(sp == 2)) {
    cl2 = col(sp)[sp == 2]
    rw2 = row(sp)[sp == 2]
    tmpy2 = rw2 + boxh/2 + boxh/10
    segments(pos[cbind(rw2, cl2)]     + boxw/2, tmpy2,
             pos[cbind(rw2, cl2 + 1)] - boxw/2, tmpy2)
  }
  
  ## Lines from offspring to parents
  ## NB: If vsep2 = T, parents are two rows above (Hack used in plot.list().)
  chRows = if(vsep2 && maxlev > 1) seq_len(maxlev-2) + 2 else seq_len(maxlev-1) + 1
  for(i in chRows) {
    parentRow = if(vsep2) i-2 else i-1
    zed = unique.default(plist$fam[i,  ]) # MDV: use unique.default
    zed = zed[zed > 0]  #list of family ids
    
    for(fam in zed) {
      xx = pos[parentRow, fam + 0:1]
      parentx = mean(xx)   #midpoint of parents
      
      # Draw the uplines
      who = (plist$fam[i,] == fam) #The kids of interest
      if (is.null(plist$twins))
        target = pos[i,who]
      else {
        twin.to.left = (c(0, plist$twins[i,who])[1:sum(who)])
        temp = cumsum(twin.to.left == 0) #increment if no twin to the left
        # 5 sibs, middle 3 are triplets gives 1,2,2,2,3
        # twin, twin, singleton gives 1,1,2,2,3
        tcount = table(temp)
        target = rep(tapply(pos[i,who], temp, mean), tcount)
      }
      
      yy = rep(i, sum(who))
      segments(pos[i,who], yy, target, yy-legh)
      
      ## Draw midpoint MZ twin line
      if (any(plist$twins[i,who] == 1)) {
        who2 = which(plist$twins[i,who] == 1)
        temp1 = (pos[i, who][who2] + target[who2])/2
        temp2 = (pos[i, who][who2+1] + target[who2])/2
        yy = rep(i, length(who2)) - legh/2
        segments(temp1, yy, temp2, yy)
      }
      
      # Add a question mark for those of unknown zygosity
      if (any(plist$twins[i,who] == 3)) {
        who2 = which(plist$twins[i,who] == 3)
        temp1 = (pos[i, who][who2] + target[who2])/2
        temp2 = (pos[i, who][who2+1] + target[who2])/2
        yy = rep(i, length(who2)) - legh/2
        text.default((temp1+temp2)/2, yy, '?')
      }
      
      # Add the horizontal line
      segments(min(target), i-legh, max(target), i-legh)
      
      # Draw line to parents. MDV: `pconnect` set to 0.5
      if (diff(range(target)) < 2*pconnect)
        x1 = mean(range(target))
      else
        x1 = pmax(min(target) + pconnect, pmin(max(target) - pconnect, parentx))
      
      # MDV: `branch` set to 0.6
      y1 = i - legh
      y2 = parentRow + boxh/2
      x2 = parentx
      ydelta = ((y2 - y1) * branch)/2
      segments(c(x1, x1, x2), c(y1, y1 + ydelta, y2 - ydelta),
               c(x1, x2, x2), c(y1 + ydelta, y2 - ydelta, y2))
    }
  } ## end of parent-child lines
  
  
  # Duplication arcs
  arcconnect = function(x, y) {
    xx = seq(x[1], x[2], length = 15)
    yy = seq(y[1], y[2], length = 15) + (seq(-7, 7))^2/98 - .5
    lines(xx, yy, lty = 2)
  }
  
  for (id in nid[duplicated.default(nid, incomparables = 0)]) { # faster than unique
    indx = which(nid == id)
    if (length(indx) > 1) {  # subject is a multiple
      tx = pos[indx]
      ty = row(pos)[indx]
      
      # MDV: Clarify code. Connect sequentially left -> right
      ord = order(tx)
      tx = tx[ord]
      ty = ty[ord]
      for (j in 1:(length(indx) - 1))
        arcconnect(tx[j + 0:1], ty[j + 0:1])
    }
  }
  
  ## Finish
  ckall = seq_len(n)[-nid]
  if(length(ckall>0))
    cat('Did not plot the following people:', ckall,'\n')
  
}


#' @rdname plotmethods
#' @importFrom graphics segments points text.default
#' @export
.annotatePed = function(alignment, annotation, scaling, font = NULL, fam = NULL,
                        col = NULL, colUnder = 1, colInside = 1, colAbove = 1,
                        cex.main = NULL, font.main = NULL, col.main = NULL, line.main = NA, ...) {
  
  nInd = alignment$nInd
  xall = alignment$xall
  yall = alignment$yall
  plotord = alignment$plotord
  
  boxh = scaling$boxh
  boxw = scaling$boxw
  labh = scaling$labh
  cex = scaling$cex
  
  title = annotation$title
  deceased = annotation$deceasedTF
  carrier = annotation$carrierTF
  proband = annotation$probandTF
  textUnder = annotation$textUnder
  textAnnot = annotation$textAnnot
  textInside = annotation$textInside
  textAbove = annotation$textAbove
  col = annotation$colvec
  
  # Add title
  if(!is.null(title)) {
    title(title, cex.main = cex.main, col.main = col.main,
          font.main = font.main, line = line.main, family = fam, xpd = NA)
  }
  
  # Deceased
  if(any(deceased)) {
    idx = which(deceased[plotord])
    ids = plotord[idx]
    segments(xall[idx] - .6*boxw, yall[idx] + 1.1*boxh,
             xall[idx] + .6*boxw, yall[idx] - 0.1*boxh, col = col[ids])
  }
  
  # Carrier dots
  if(any(carrier)) {
    idx = which(carrier[plotord])
    ids = plotord[idx]
    points(xall[idx], yall[idx] + boxh/2, pch = 16, cex = cex, col = col[ids])
  }
  
  # Proband arrow
  if(any(proband)) {
    pos.arrow = "bottomleft" # Hard coded for now
    
    mod = switch(pos.arrow,
                 bottomleft = list(x = -1, y = 1),
                 bottomright = list(x = 1, y = 1),
                 topleft = list(x = -1, y = 0),
                 topright = list(x = 1, y = 0))
    
    idx = which(proband[plotord])
    ids = plotord[idx]
    corner.x = xall[idx] + .5*mod$x * boxw
    corner.y = yall[idx] +    mod$y * boxh
    arrows(x0 = corner.x + 1.7*mod$x * boxw,
           y0 = corner.y + 0.9*mod$y * boxh - 0.9*(1-mod$y) * boxh,
           x1 = corner.x + 0.5*mod$x * boxw,
           y1 = corner.y,
           lwd = 1.2, length = .15)
  }
  
  # Colour vector
  if (length(col) == 1)
    col = rep(col, nInd)
  
  # Main labels
  text.default(xall, yall + boxh + labh * 0.7, textUnder[plotord], col = colUnder,
               cex = cex, adj = c(.5, 1), font = font, family = fam, xpd = NA)
  
  # Text inside symbols
  if(!is.null(textInside)) {
    text.default(xall, yall + boxh/2, labels = textInside[plotord], cex = cex, col = colInside,
                 font = font, family = fam)
  }
  
  # Text above symbols
  if(!is.null(textAbove)) {
    if(is.null(font) && any(startsWith(textAbove, "f =")))
      fontAbove = 3
    else
      fontAbove = font
    text.default(xall, yall, labels = textAbove[plotord], cex = cex, col = colAbove,
                 family = fam, font = fontAbove, pos = 3, offset = 0.5, xpd = NA)
  }
  
  
  if(!is.null(textAnnot)) {
    .addTxt(textAnnot[["topleft"]],     xall-boxw/2, yall,        pos = 2, plotord)
    .addTxt(textAnnot[["top"]],         xall,        yall,        pos = 3, plotord)
    .addTxt(textAnnot[["topright"]],    xall+boxw/2, yall,        pos = 4, plotord)
    .addTxt(textAnnot[["left"]],        xall-boxw/2, yall+boxh/2, pos = 2, plotord)
    .addTxt(textAnnot[["right"]],       xall+boxw/2, yall+boxh/2, pos = 4, plotord)
    .addTxt(textAnnot[["bottomleft"]],  xall-boxw/2, yall+boxh,   pos = 2, plotord)
    .addTxt(textAnnot[["bottom"]],      xall,        yall+boxh,   pos = 1, plotord)
    .addTxt(textAnnot[["bottomright"]], xall+boxw/2, yall+boxh,   pos = 4, plotord)
    .addTxt(textAnnot[["inside"]],      xall,        yall+boxh/2, pos = NULL, plotord)
  }
}


.addTxt = function(args, x, y, pos, plotord) {
  if(is.null(args))
    return()
  txt = args[[1]][plotord]
  do.call(text.default, c(list(x=x,y=y,labels=txt,pos=pos), args[-1]))
}

# Function fixing pedigree alignment of 3/4-siblings and similar
# Founders with two (or more) spouses on the same level should be placed between
.fix34 = function(x, k2ped, plist = NULL, packed = TRUE, width = 10, align = c(1.5, 2)) {
  
  # Large pedigrees: return unchanged
  if(length(x$ID) > 30)
    return(plist)
  
  fouInt = founders(x, internal = TRUE)
  nid = plist$nid
  
  # If no duplicated founders, return unchanged
  dups = duplicated.default(nid, incomparables = 0)
  if(!length(.myintersect(fouInt, nid[dups])))
    return(plist)
  
  # List of spouses
  ALLSP = vector(mode = "list", length = length(x$ID))
  ALLSP[fouInt] = lapply(fouInt, function(i) spouses(x, i, internal = TRUE))
  
  # Founders with multiple spouses
  fou2 = fouInt[lengths(ALLSP[fouInt]) > 1]
  
  # Go row by row in nid
  SP = NULL
  for(k in 2:length(plist$n)) {
    rw = nid[k, ]
    
    for(id in .myintersect(fou2, rw)) {
      s = .myintersect(ALLSP[[id]], rw) # spouses on that level
      if(length(s) > 1)
        SP = rbind(SP, c(s[1], id, 0), c(id, s[2], 0))
    }
  }
  
  # If hints added, redo alignment
  if(!is.null(SP)) {
    hints = list(order = seq_along(x$ID), spouses = SP)
    plist = kinship2::align.pedigree(k2ped, packed = packed, width = width, align = align, hints = hints)
  }
  
  plist
}

# Convert plot parameter (col/fill/lty/lwd) to full vector in pedigree order
.prepPlotarg = function(x, par, default) {
  nInd = length(x$ID)
  nms = names(par)
  
  if(!is.list(par)) {
    if(!is.null(nms)) {
      vec = rep(default, length = nInd)
      ids = intersect(x$ID, nms)
      vec[internalID(x, ids)] = par[ids]
    }
    else {
      vec = rep(par, length = nInd)
    }
  }
  else { # E.g. list(red = 1:2, "3" = males)
    vec = rep(default, nInd)
    for(cc in nms) {
      v = par[[cc]]
      if(is.function(v))
        ids = v(x)
      else
        ids = intersect(x$ID, v)
      
      idsInt = internalID(x, ids)
      if(length(idsInt))
        vec[idsInt] = cc
    }
  }
  
  vec
}


.spouseOrder = function(x, plotorder) {
  if(!is.ped(x))
    stop2("Spouse ordering is not implemented for ped lists")
  
  if(!is.list(plotorder))
    plotorder = list(plotorder)
  
  allPairs = list()
  for(ids in plotorder) {
    idsInt = internalID(x, ids)
    newPairs = lapply(2:length(idsInt), function(i) idsInt[(i-1):i])
    allPairs = c(allPairs, newPairs)
  }
  
  for(p in allPairs) {
    if(!p[1] %in% spouses(x, p[2], internal = TRUE))
      stop2(sprintf("'%s' is not spouse of '%s'", x$ID[p[2]], x$ID[p[1]]))
  }
  
  spouse = cbind(do.call(rbind, allPairs), 0)
  list(order = seq_along(x$ID), spouse = spouse)
}

ped = function(id, fid, mid, sex, famid = "", reorder = TRUE, validate = TRUE,
               detectLoops = TRUE, isConnected = FALSE, verbose = FALSE) {
  
  # Check input
  n = length(id)
  
  if(n == 0)
    stop2("`id` vector has length 0")
  if(length(fid) != n)
    stop2(sprintf("Incompatible input: length(id) = %d, but length(fid) = %d", n, length(fid)))
  if(length(mid) != n)
    stop2(sprintf("Incompatible input: length(id) = %d, but length(mid) = %d", n, length(mid)))
  if(length(sex) != n)
    stop2(sprintf("Incompatible input: length(id) = %d, but length(sex) = %d", n, length(sex)))
  
  # Coerce
  id = as.character(id)
  fid = as.character(fid)
  mid = as.character(mid)
  famid = as.character(famid)
  
  # Duplicated IDs
  if(anyDuplicated.default(id) > 0)
    stop2("Duplicated entry in `id` vector: ", id[duplicated(id)])
  
  # Parental index vectors (integer).
  missing = c("", "0", NA)
  FIDX = match(fid, id)
  FIDX[fid %in% missing] = 0L
  
  MIDX = match(mid, id)
  MIDX[mid %in% missing] = 0L
  
  if(any(is.na(FIDX)))
    stop2("`fid` entry does not appear in `id` vector: ", fid[is.na(FIDX)])
  if(any(is.na(MIDX)))
    stop2("`mid` entry does not appear in `id` vector: ", mid[is.na(MIDX)])
  
  if(all(FIDX + MIDX > 0))
    stop2("Pedigree has no founders")
  
  if(length(famid) != 1)
    stop2("`famid` must be a character string: ", famid)
  
  # Check for illegal entries in `sex``
  if(!all(sex %in% 0:2))
    stop2("Illegal sex: ", .mysetdiff(sex, 0:2))
  sex = as.integer(sex)
  
  # Connected components
  if(!isConnected) {
    
    # Identify components
    comps = connectedComponents(id, fidx = FIDX, midx = MIDX)
    
    if(length(comps) > 1) {
      famids = paste0(famid, "_comp", seq_along(comps))
      
      pedlist = lapply(seq_along(comps), function(i) {
        idx = match(comps[[i]], id)
        ped(id = id[idx], fid = fid[idx], mid = mid[idx],
            sex = sex[idx], famid = famids[i], reorder = reorder,
            validate = validate, detectLoops = detectLoops,
            isConnected = TRUE, verbose = verbose)
      })
      
      return(structure(pedlist, names = famids, class = c("pedList", "list")))
    }
  }
  
  # Initialise ped object
  x = newPed(id, FIDX, MIDX, sex, famid, detectLoops = FALSE) # TODO
  
  # Detect loops (by trying to find a peeling order)
  if(detectLoops)
    x$UNBROKEN_LOOPS = hasUnbrokenLoops(x)
  
  if(validate)
    validatePed(x)
  
  # reorder so that parents precede their children
  if(reorder)
    x = parentsBeforeChildren(x)
  
  x
}


#' @export
#' @rdname ped
singleton = function(id = 1, sex = 1, famid = "") {
  if (length(id) != 1)
    stop2("`id` must have length 1")
  sex = validate_sex(sex, nInd = 1)
  newPed(ID = as.character(id), FIDX = 0L, MIDX = 0L, SEX = sex,
         FAMID = famid, detectLoops = FALSE)
}

newPed = function(ID, FIDX, MIDX, SEX, FAMID, detectLoops = TRUE) {
  if(!all(is.character(ID), is.integer(FIDX), is.integer(MIDX),
          is.integer(SEX), is.character(FAMID)))
    stop2("Type error in the creation of `ped` object")
  
  # Initialise ped object
  x = list(ID = ID,
           FIDX = FIDX,
           MIDX = MIDX,
           SEX = SEX,
           FAMID = FAMID,
           UNBROKEN_LOOPS = NA,
           LOOP_BREAKERS = NULL,
           FOUNDER_INBREEDING = NULL,
           MARKERS = NULL)
  
  if(length(ID) == 1) {
    class(x) = c("singleton", "ped")
    x$UNBROKEN_LOOPS = FALSE
    return(x)
  }
  
  class(x) = "ped"
  if(detectLoops)
    x$UNBROKEN_LOOPS = hasUnbrokenLoops(x)
  
  x
}
validatePed = function(x = NULL, id = NULL, fid = NULL, mid = NULL, sex = NULL) {
  if(!is.null(x)) {
    ID = x$ID; FIDX = x$FIDX; MIDX = x$MIDX; SEX = x$SEX; FAMID = x$FAMID
  }
  else {
    ID = as.character(id); FIDX = match(fid, id, nomatch = 0L); MIDX = match(mid, id, nomatch = 0L);
    SEX = as.integer(sex); FAMID = ""
  }
  
  n = length(ID)
  
  # Type verification (mainly for developer)
  stopifnot2(is.character(ID), is.integer(FIDX), is.integer(MIDX), is.integer(SEX),
             is.character(FAMID), is.singleton(x) == (n == 1))
  
  # Other verifications that don't need friendly messages at this point
  # (since they should be caught earlier during construction)
  stopifnot2(n > 0, length(FIDX) == n, length(MIDX) == n, length(SEX) == n,
             all(FIDX >= 0), all(MIDX >= 0), all(FIDX <= n), all(MIDX <= n),
             length(FAMID) == 1)
  
  errs = character(0)
  
  # Either 0 or 2 parents
  has1parent = (FIDX > 0) != (MIDX > 0)
  if (any(has1parent))
    errs = c(errs, paste("Individual", ID[has1parent], "has exactly 1 parent; this is not allowed"))
  
  # Sex
  if (!all(SEX %in% 0:2))
    errs = c(errs, paste("Illegal sex:", unique(setdiff(SEX, 0:2))))
  
  # Self ancestry
  self_anc = any_self_ancestry(list(ID = ID, FIDX = FIDX, MIDX = MIDX))
  if(length(self_anc) > 0)
    errs = c(errs, paste("Individual", self_anc, "is their own ancestor"))
  
  # If singleton: return here
  # if(n == 1) return()
  
  # Duplicated IDs
  if(anyDuplicated.default(ID) > 0)
    errs = c(errs, paste("Duplicated ID label:", ID[duplicated(ID)]))
  
  # Female fathers
  if(any(SEX[FIDX] == 2)) {
    female_fathers_int = intersect(which(SEX == 2), FIDX) # note: zeroes in FIDX disappear
    first_child = ID[match(female_fathers_int, FIDX)]
    errs = c(errs, paste("Individual", ID[female_fathers_int],
                         "is female, but appear as the father of", first_child))
  }
  
  # Male mothers
  if(any(SEX[MIDX] == 1)) {
    male_mothers_int = intersect(which(SEX == 1), MIDX) # note: zeroes in MIDX disappear
    first_child = ID[match(male_mothers_int, MIDX)]
    errs = c(errs, paste("Individual", ID[male_mothers_int],
                         "is male, but appear as the mother of", first_child))
  }
  
  # Connected?
  #if (all(c(FIDX, MIDX) == 0))
  #    message("Pedigree is not connected.")
  
  if(length(errs) > 0) {
    errs = c("Malformed pedigree.", errs)
    stop2(paste0(errs, collapse = "\n "))
  }
  
  invisible(NULL)
}


any_self_ancestry = function(x) {
  ID = x$ID
  FIDX = x$FIDX
  MIDX = x$MIDX
  
  n = length(ID)
  nseq = seq_len(n)
  
  # Quick check if anyone is their own parent
  self_parent = (nseq == FIDX) | (nseq == MIDX)
  if(any(self_parent))
    return(ID[self_parent])
  
  fou_int = which(FIDX == 0)
  OK = rep(FALSE, n)
  OK[fou_int] = TRUE
  
  # TODO: works, but not optimised for speed
  for(i in nseq) { # note that i is not used
    parents = which(OK)
    children = which(FIDX %in% parents | MIDX %in% parents)
    
    fatherOK = OK[FIDX[children]]
    motherOK = OK[MIDX[children]]
    childrenOK = children[fatherOK & motherOK]
    
    # If these were already ok, there is nothing more to do
    if(all(OK[childrenOK]))
      break
    
    OK[childrenOK] = TRUE
  }
  ID[!OK]
}

#conversion to ped objects 
as.ped(
  x,
  famid_col = NA,
  id_col = NA,
  fid_col = NA,
  mid_col = NA,
  sex_col = NA,
  marker_col = NA,
  locusAttributes = NULL,
  missing = 0,
  sep = NULL,
  sexCodes = NULL,
  addMissingFounders = FALSE,
  validate = TRUE,
  verbose = TRUE,
  ...
)
getSex(x, ids = NULL, named = FALSE)

setSex(x, ids = NULL, sex)

swapSex(x, ids, verbose = TRUE)
nuclearPed(nch = 1, sex = 1, father = "1", mother = "2", children = NULL)

halfSibPed(
  nch1 = 1,
  nch2 = 1,
  sex1 = 1,
  sex2 = 1,
  type = c("paternal", "maternal")
)

linearPed(n, sex = 1)

cousinPed(
  degree = 1,
  removal = 0,
  side = c("right", "left"),
  half = FALSE,
  symmetric = FALSE,
  child = FALSE
)

avuncularPed(
  top = c("uncle", "aunt"),
  bottom = c("nephew", "niece"),
  side = c("right", "left"),
  type = c("paternal", "maternal"),
  removal = 1,
  half = FALSE
)

halfCousinPed(
  degree = 1,
  removal = 0,
  side = c("right", "left"),
  symmetric = FALSE,
  child = FALSE
)
reorderPed(x, neworder = NULL, internal = FALSE)

parentsBeforeChildren(x)

hasParentsBeforeChildren(x)

foundersFirst(x)

internalID(x, ids, errorIfUnknown = TRUE)

addChildren(
  x,
  father = NULL,
  mother = NULL,
  nch = NULL,
  sex = 1L,
  ids = NULL,
  verbose = TRUE
)

addChild(x, parents, id = NULL, sex = 1, verbose = TRUE)

addSon(x, parents, id = NULL, verbose = TRUE)

addDaughter(x, parents, id = NULL, verbose = TRUE)

addParents(x, id, father = NULL, mother = NULL, verbose = TRUE)

removeIndividuals(
  x,
  ids,
  remove = c("descendants", "ancestors"),
  returnLabs = FALSE,
  verbose = TRUE
)

trim(x, uninformative, verbose = TRUE)

branch(x, id)

founders(x, internal = FALSE)

nonfounders(x, internal = FALSE)

leaves(x, internal = FALSE)

males(x, internal = FALSE)

females(x, internal = FALSE)

typedMembers(x, internal = FALSE)

untypedMembers(x, internal = FALSE)

father(x, id, internal = FALSE)

mother(x, id, internal = FALSE)

children(x, id, internal = FALSE)

spouses(x, id, internal = FALSE)

unrelated(x, id, internal = FALSE)

parents(x, id, internal = FALSE)

grandparents(x, id, degree = 2, internal = FALSE)

siblings(x, id, half = NA, internal = FALSE)

nephews_nieces(x, id, removal = 1, half = NA, internal = FALSE)

niblings(x, id, half = NA, internal = FALSE)

piblings(x, id, half = NA, internal = FALSE)

ancestors(x, id, maxGen = Inf, inclusive = FALSE, internal = FALSE)

commonAncestors(x, ids, maxGen = Inf, inclusive = FALSE, internal = FALSE)

descendants(x, id, maxGen = Inf, inclusive = FALSE, internal = FALSE)

commonDescendants(x, ids, maxGen = Inf, inclusive = FALSE, internal = FALSE)

descentPaths(x, ids = founders(x), internal = FALSE)

randomPed(n, founders = 2, maxDirectGap = 1, selfing = FALSE, seed = NULL)
# Arguments
# 
# n	
# A positive integer: the total number of individuals. Must be at least 3.
# founders	
# A positive integer: the number of founders. Must be at least 2 unless selfing is allowed.
# maxDirectGap	
# An integer; the maximum distance between direct descendants allowed to mate. For example, the default value of 1 allows parent-child mating, but not grandparent-grandchild. Use Inf or NULL for no restrictions.
# selfing	
# A logical indicating if selfing is allowed. Default: FALSE.
# seed	
# An integer seed for the random number generator (optional).

relabel(
  x,
  new = "asPlot",
  old = labels(x),
  reorder = FALSE,
  returnLabs = FALSE,
  .alignment = NULL
)

#---------------------EXEMPLES -----------------
#' # Singleton
#' plot(singleton(1))
#' 
#' # Trio
#' x = nuclearPed(father = "fa", mother = "mo", child = "boy")
#' plot(x)
#' 
#' #' # Modify margins
#' plot(x, margins = 6)
#' plot(x, margins = c(0,0,6,6)) # b,l,t,r
#' 
#' # Larger text and symbols
#' plot(x, cex = 1.5)
#' 
#' # Enlarge symbols only
#' plot(x, symbolsize = 1.5)
#' 
#' # Various annotations
#' plot(x, hatched = "boy", starred = "fa", deceased = "mo", title = "Fam 1")
#' 
#' # Swap spouse order
#' plot(x, spouseOrder = c("mo", "fa"))
#' 
#' #----- ID labels -----
#' 
#' # Label only some members
#' plot(x, labs = c("fa", "mo"))
#' 
#' # Label males only
#' plot(x, labs = males)
#' 
#' # Rename some individuals
#' plot(x, labs = c(FATHER = "fa", "boy"))
#' 
#' # By default, long names are folded to width ~12 characters
#' plot(x, labs = c("Very long father's name" = "fa"), margin = 2)
#' 
#' # Folding width may be adjusted ...
#' plot(x, labs = c("Very long father's name" = "fa"), foldLabs = 6)
#' 
#' # ... or switched off (requires larger margin!)
#' plot(x, labs = c("Very long father's name" = "fa"), foldLabs = FALSE)
#' 
#' # By default, labels are trimmed for initial/trailing line breaks ...
#' plot(x, labs = c("\nFA" = "fa"))
#' 
#' # ... but this can be overridden
#' plot(x, labs = c("\nFA" = "fa"), trimLabs = FALSE)
#' 
#' #----- Colours -----
#' 
#' plot(x, col = c(fa = "red"), fill = c(mo = "green", boy = "blue"))
#' 
#' # Non-black hatch colours are specified with the `fill` argument
#' plot(x, hatched = labels, fill = c(boy = "red"))
#' 
#' # Use functions to specify colours
#' plot(x, fill = list(red = leaves, blue = ancestors(x, "boy")))
#' 
#' #----- Symbol line types and widths -----
#' 
#' # Dotted, thick symbols
#' plot(x, lty = 3, lwd = 4, cex = 2)
#' 
#' # Detailed specification of line types and width
#' plot(x, lty = list(dashed = founders), lwd = c(boy = 4))
#' 
#' #----- Genotypes -----
#' 
#' x = nuclearPed(father = "fa", mother = "mo", child = "boy") |>
#'   addMarker(fa = "1/1", boy = "1/2", name = "SNP") |>
#'   addMarker(boy = "a/b")
#' 
#' # Show genotypes for first marker
#' plot(x, marker = 1)
#' 
#' # Show empty genotypes for untyped individuas
#' plot(x, marker = 1, showEmpty = TRUE)
#' 
#' # Markers can also be called by name
#' plot(x, marker = "SNP")
#' 
#' # Multiple markers
#' plot(x, marker = 1:2)
#' 
#' #----- Further text annotation -----
#' 
#' # Founder inbreeding is shown by default
#' xinb = x |> setFounderInbreeding("mo", value = 0.1)
#' plot(xinb)
#' 
#' # ... but can be suppressed
#' plot(xinb, fouInb = NULL)
#' 
#' # Text can be placed around and inside symbols
#' plot(x, textAnnot = list(topright = 1:3, inside = LETTERS[1:3]))
#' 
#' # Use lists to add further options; see `?text()`
#' plot(x, margin = 2, textAnnot = list(
#'   topright = list(1:3, cex = 0.8, col = 2, font = 2, offset = 0.1),
#'   left = list(c(boy = "comment"), cex = 2, col = 4, offset = 2, srt = 20)))
#' 
#' # Exhaustive list of annotation positions
#' plot(singleton(1), cex = 3, textAnnot = list(top="top", left="left",
#'                                              right="right", bottom="bottom", topleft="topleft", topright="topright",
#'                                              bottomleft="bottomleft", bottomright="bottomright", inside="inside"))
#' 
#' #----- Special pedigrees -----
#' 
#' # Plot as DAG (directed acyclic graph)
#' plot(x, arrows = TRUE, title = "DAG")
#' 
#' # Medical pedigree
#' plot(x, aff = "boy", carrier = "mo")
#' 
#' # Miscarriage
#' plot(x, miscarriage = "boy", deceased = "boy", labs = founders)
#' 
#' # Twins
#' x = nuclearPed(children = c("tw1", "tw2", "tw3"))
#' plot(x, twins = data.frame(id1 = "tw1", id2 = "tw2", code = 1)) # MZ
#' plot(x, twins = data.frame(id1 = "tw1", id2 = "tw2", code = 2)) # DZ
#' 
#' # Triplets
#' plot(x, twins = data.frame(id1 = c("tw1", "tw2"),
#'                            id2 = c("tw2", "tw3"),
#'                            code = 2))
#' 
#' # Selfing
#' plot(selfingPed(2))
#' 
#' # Complex pedigree: Quadruple half first cousins
#' plot(quadHalfFirstCousins())
#' 
#' # Straight legs
#' plot(quadHalfFirstCousins(), align = c(0,0))
#' 
#' # Lists of multiple pedigree
#' plot(list(singleton(1), nuclearPed(1), linearPed(2)))
#' 
#' # Use of `drawPed()`
#' dat = plot(nuclearPed(), draw = FALSE)
#' drawPed(dat$alignment, dat$annotation, dat$scaling)
