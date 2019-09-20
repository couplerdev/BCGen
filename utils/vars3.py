vlist = { }
for line in open("vars.txt"):
	sl = line.split("=")
	name = sl[0][23:26]
	vl = sl[1].strip().split(":")
	if name in vlist :
		if vl[0] != '' :
			vlist[name] += vl
	else :
		if vl[0] != '' :
			vlist[name] = vl

tlist = { }
for key in vlist :
	if key[0:2] == 'x2' :
		target = key[2]
		slist = { }
		for var in vlist[key] :
			for src in vlist :
				if src[1:3] == '2x' :
					source = src[0]
					if var in vlist[src] :
						if source in slist :
							slist[source] += ':' + var
						else :
							slist[source] = var
		tlist[target] = slist
	
for key in tlist :
	print(key)
	slist = tlist[key]
	for src in slist :
		print('    ' + src + '=', end = '')
		print(slist[src])
