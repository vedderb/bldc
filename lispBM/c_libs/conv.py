import sys,getopt,binascii

filename = ""
name = "test"

opts,args = getopt.getopt(sys.argv[1:],'f:n:')
for o,a in opts:
	if o == '-f':
		filename = a
	if o == '-n':
		name = a

with open(filename, "rb") as f:
	hexdata = f.read().hex()
	tokens = [hexdata[i:i+2] for i in range(0, len(hexdata), 2)]
	res = "(def " + name + " [\n"
	cnt = 0
	for c in tokens:
		res += "0x" + c
		cnt = cnt + 1
		if cnt == 20:
			cnt = 0
			res += "\n"
		else:
			res += " "
	
	if res[-1] == '\n':
		res += "])\n"
	else:
		res += "\n])\n"
	print(res)

