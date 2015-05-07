PERL	= perl
AWK	= awk
UCS2VGA	= $(PERL) ./ucstoany.pl +f
UNI2TXT	= $(AWK) '{ printf("0x%02X\tU+%s\n", FNR-1, $$1) }'
BDF2PSF	= $(PERL) ./bdftopsf.pl +u
BDF2RAW	= $(PERL) ./bdftopsf.pl -h
UCS2X11	= $(PERL) ./ucstoany.pl
BDF2PCF	= bdftopcf

REG_8859_1  = ISO8859 1
REG_8859_2  = ISO8859 2
REG_8859_5  = ISO8859 5
REG_8859_7  = ISO8859 7
REG_8859_9  = ISO8859 9
REG_W_1251  = Microsoft CP1251
REG_8859_13 = ISO8859 13
REG_8859_15 = ISO8859 15
REG_8859_16 = ISO8859 16
REG_IBM_437 = IBM CP437
REG_KOI8_R  = KOI8 R
REG_KOI8_U  = KOI8 U
REG_BG_MIK  = Bulgarian MIK
REG_PT_154  = Paratype PT154
REG_XOS4_2  = XOS4 2
REG_U_10646 = ISO10646 1

PSF_8859_1  = ter-112n.psf ter-114n.psf ter-114b.psf ter-114v.psf ter-116n.psf ter-116b.psf ter-116v.psf ter-118n.psf ter-118b.psf ter-120n.psf ter-120b.psf ter-122n.psf ter-122b.psf ter-124n.psf ter-124b.psf ter-128n.psf ter-128b.psf ter-132n.psf ter-132b.psf
PSF_8859_2  = ter-212n.psf ter-214n.psf ter-214b.psf ter-214v.psf ter-216n.psf ter-216b.psf ter-216v.psf ter-218n.psf ter-218b.psf ter-220n.psf ter-220b.psf ter-222n.psf ter-222b.psf ter-224n.psf ter-224b.psf ter-228n.psf ter-228b.psf ter-232n.psf ter-232b.psf
PSF_8859_7  = ter-712n.psf ter-714n.psf ter-714b.psf ter-714v.psf ter-716n.psf ter-716b.psf ter-716v.psf ter-718n.psf ter-718b.psf ter-720n.psf ter-720b.psf ter-722n.psf ter-722b.psf ter-724n.psf ter-724b.psf ter-728n.psf ter-728b.psf ter-732n.psf ter-732b.psf
PSF_8859_9  = ter-912n.psf ter-914n.psf ter-914b.psf ter-914v.psf ter-916n.psf ter-916b.psf ter-916v.psf ter-918n.psf ter-918b.psf ter-920n.psf ter-920b.psf ter-922n.psf ter-922b.psf ter-924n.psf ter-924b.psf ter-928n.psf ter-928b.psf ter-932n.psf ter-932b.psf
PSF_W_1251  = ter-c12n.psf ter-c14n.psf ter-c14b.psf ter-c14v.psf ter-c16n.psf ter-c16b.psf ter-c16v.psf ter-c18n.psf ter-c18b.psf ter-c20n.psf ter-c20b.psf ter-c22n.psf ter-c22b.psf ter-c24n.psf ter-c24b.psf ter-c28n.psf ter-c28b.psf ter-c32n.psf ter-c32b.psf
PSF_8859_13 = ter-d12n.psf ter-d14n.psf ter-d14b.psf ter-d14v.psf ter-d16n.psf ter-d16b.psf ter-d16v.psf ter-d18n.psf ter-d18b.psf ter-d20n.psf ter-d20b.psf ter-d22n.psf ter-d22b.psf ter-d24n.psf ter-d24b.psf ter-d28n.psf ter-d28b.psf ter-d32n.psf ter-d32b.psf
PSF_8859_16 = ter-g12n.psf ter-g14n.psf ter-g14b.psf ter-g14v.psf ter-g16n.psf ter-g16b.psf ter-g16v.psf ter-g18n.psf ter-g18b.psf ter-g20n.psf ter-g20b.psf ter-g22n.psf ter-g22b.psf ter-g24n.psf ter-g24b.psf ter-g28n.psf ter-g28b.psf ter-g32n.psf ter-g32b.psf
PSF_IBM_437 = ter-i12n.psf ter-i14n.psf ter-i14b.psf ter-i14v.psf ter-i16n.psf ter-i16b.psf ter-i16v.psf ter-i18n.psf ter-i18b.psf ter-i20n.psf ter-i20b.psf ter-i22n.psf ter-i22b.psf ter-i24n.psf ter-i24b.psf ter-i28n.psf ter-i28b.psf ter-i32n.psf ter-i32b.psf
PSF_KOI8_RV = ter-k14n.psf ter-k14b.psf ter-k14v.psf ter-k16n.psf ter-k16b.psf ter-k16v.psf
PSF_KOI8_R  = ter-k12n.psf ter-k18n.psf ter-k18b.psf ter-k20n.psf ter-k20b.psf ter-k22n.psf ter-k22b.psf ter-k24n.psf ter-k24b.psf ter-k28n.psf ter-k28b.psf ter-k32n.psf ter-k32b.psf
PSF_BG_MIK  = ter-m12n.psf ter-m14n.psf ter-m14b.psf ter-m14v.psf ter-m16n.psf ter-m16b.psf ter-m16v.psf ter-m18n.psf ter-m18b.psf ter-m20n.psf ter-m20b.psf ter-m22n.psf ter-m22b.psf ter-m24n.psf ter-m24b.psf ter-m28n.psf ter-m28b.psf ter-m32n.psf ter-m32b.psf
PSF_PT_154  = ter-p12n.psf ter-p14n.psf ter-p14b.psf ter-p14v.psf ter-p16n.psf ter-p16b.psf ter-p16v.psf ter-p18n.psf ter-p18b.psf ter-p20n.psf ter-p20b.psf ter-p22n.psf ter-p22b.psf ter-p24n.psf ter-p24b.psf ter-p28n.psf ter-p28b.psf ter-p32n.psf ter-p32b.psf
PSF_KOI8_UV = ter-u14n.psf ter-u14b.psf ter-u14v.psf ter-u16n.psf ter-u16b.psf ter-u16v.psf
PSF_KOI8_U  = ter-u12n.psf ter-u18n.psf ter-u18b.psf ter-u20n.psf ter-u20b.psf ter-u22n.psf ter-u22b.psf ter-u24n.psf ter-u24b.psf ter-u28n.psf ter-u28b.psf ter-u32n.psf ter-u32b.psf
PSF_XOS4_2  = ter-v12n.psf ter-v14n.psf ter-v14b.psf ter-v14v.psf ter-v16n.psf ter-v16b.psf ter-v16v.psf ter-v18n.psf ter-v18b.psf ter-v20n.psf ter-v20b.psf ter-v22n.psf ter-v22b.psf ter-v24n.psf ter-v24b.psf ter-v28n.psf ter-v28b.psf ter-v32n.psf ter-v32b.psf
PSF = $(PSF_8859_1) $(PSF_8859_2) $(PSF_8859_7) $(PSF_8859_9) $(PSF_W_1251) $(PSF_8859_13) $(PSF_8859_16) $(PSF_IBM_437) $(PSF_KOI8_RV) $(PSF_KOI8_R) $(PSF_BG_MIK) $(PSF_PT_154) $(PSF_KOI8_UV) $(PSF_KOI8_U) $(PSF_XOS4_2)

TXT = cp1252.txt cp1250.txt cp1253.txt cp1254.txt cp1251.txt cp1257.txt pt154.txt 8859-16.txt cp437.txt koi8-r.txt koi8-u.txt mik.txt pt254.txt

RAW_8859_1  = ter-114n.raw ter-114b.raw ter-114v.raw ter-116n.raw ter-116b.raw ter-116v.raw
RAW_8859_2  = ter-214n.raw ter-214b.raw ter-214v.raw ter-216n.raw ter-216b.raw ter-216v.raw
RAW_8859_5  = ter-514n.raw ter-514b.raw ter-514v.raw ter-516n.raw ter-516b.raw ter-516v.raw
RAW_8859_7  = ter-714n.raw ter-714b.raw ter-714v.raw ter-716n.raw ter-716b.raw ter-716v.raw
RAW_8859_9  = ter-914n.raw ter-914b.raw ter-914v.raw ter-916n.raw ter-916b.raw ter-916v.raw
RAW_W_1251  = ter-c14n.raw ter-c14b.raw ter-c14v.raw ter-c16n.raw ter-c16b.raw ter-c16v.raw
RAW_8859_13 = ter-d14n.raw ter-d14b.raw ter-d14v.raw ter-d16n.raw ter-d16b.raw ter-d16v.raw
RAW_8859_15 = ter-f14n.raw ter-f14b.raw ter-f14v.raw ter-f16n.raw ter-f16b.raw ter-f16v.raw
RAW_8859_16 = ter-g14n.raw ter-g14b.raw ter-g14v.raw ter-g16n.raw ter-g16b.raw ter-g16v.raw
RAW_IBM_437 = ter-i14n.raw ter-i14b.raw ter-i14v.raw ter-i16n.raw ter-i16b.raw ter-i16v.raw
RAW_KOI8_R  = ter-k14n.raw ter-k14b.raw ter-k14v.raw ter-k16n.raw ter-k16b.raw ter-k16v.raw
RAW_PT_154  = ter-p14n.raw ter-p14b.raw ter-p14v.raw ter-p16n.raw ter-p16b.raw ter-p16v.raw
RAW_KOI8_U  = ter-u14n.raw ter-u14b.raw ter-u14v.raw ter-u16n.raw ter-u16b.raw ter-u16v.raw
RAW = $(RAW_8859_1) $(RAW_8859_2) $(RAW_8859_5) $(RAW_8859_7) $(RAW_8859_9) $(RAW_W_1251) $(RAW_8859_13) $(RAW_8859_15) $(RAW_8859_16) $(RAW_IBM_437) $(RAW_KOI8_R) $(RAW_PT_154) $(RAW_KOI8_U)

PCF_8859_1  = ter-112n.pcf ter-112b.pcf ter-114n.pcf ter-114b.pcf ter-116n.pcf ter-116b.pcf ter-118n.pcf ter-118b.pcf ter-120n.pcf ter-120b.pcf ter-122n.pcf ter-122b.pcf ter-124n.pcf ter-124b.pcf ter-128n.pcf ter-128b.pcf ter-132n.pcf ter-132b.pcf
PCF_8859_2  = ter-212n.pcf ter-212b.pcf ter-214n.pcf ter-214b.pcf ter-216n.pcf ter-216b.pcf ter-218n.pcf ter-218b.pcf ter-220n.pcf ter-220b.pcf ter-222n.pcf ter-222b.pcf ter-224n.pcf ter-224b.pcf ter-228n.pcf ter-228b.pcf ter-232n.pcf ter-232b.pcf
PCF_8859_5  = ter-512n.pcf ter-512b.pcf ter-514n.pcf ter-514b.pcf ter-516n.pcf ter-516b.pcf ter-518n.pcf ter-518b.pcf ter-520n.pcf ter-520b.pcf ter-522n.pcf ter-522b.pcf ter-524n.pcf ter-524b.pcf ter-528n.pcf ter-528b.pcf ter-532n.pcf ter-532b.pcf
PCF_8859_7  = ter-712n.pcf ter-712b.pcf ter-714n.pcf ter-714b.pcf ter-716n.pcf ter-716b.pcf ter-718n.pcf ter-718b.pcf ter-720n.pcf ter-720b.pcf ter-722n.pcf ter-722b.pcf ter-724n.pcf ter-724b.pcf ter-728n.pcf ter-728b.pcf ter-732n.pcf ter-732b.pcf
PCF_8859_9  = ter-912n.pcf ter-912b.pcf ter-914n.pcf ter-914b.pcf ter-916n.pcf ter-916b.pcf ter-918n.pcf ter-918b.pcf ter-920n.pcf ter-920b.pcf ter-922n.pcf ter-922b.pcf ter-924n.pcf ter-924b.pcf ter-928n.pcf ter-928b.pcf ter-932n.pcf ter-932b.pcf
PCF_W_1251  = ter-c12n.pcf ter-c12b.pcf ter-c14n.pcf ter-c14b.pcf ter-c16n.pcf ter-c16b.pcf ter-c18n.pcf ter-c18b.pcf ter-c20n.pcf ter-c20b.pcf ter-c22n.pcf ter-c22b.pcf ter-c24n.pcf ter-c24b.pcf ter-c28n.pcf ter-c28b.pcf ter-c32n.pcf ter-c32b.pcf
PCF_8859_13 = ter-d12n.pcf ter-d12b.pcf ter-d14n.pcf ter-d14b.pcf ter-d16n.pcf ter-d16b.pcf ter-d18n.pcf ter-d18b.pcf ter-d20n.pcf ter-d20b.pcf ter-d22n.pcf ter-d22b.pcf ter-d24n.pcf ter-d24b.pcf ter-d28n.pcf ter-d28b.pcf ter-d32n.pcf ter-d32b.pcf
PCF_8859_15 = ter-f12n.pcf ter-f12b.pcf ter-f14n.pcf ter-f14b.pcf ter-f16n.pcf ter-f16b.pcf ter-f18n.pcf ter-f18b.pcf ter-f20n.pcf ter-f20b.pcf ter-f22n.pcf ter-f22b.pcf ter-f24n.pcf ter-f24b.pcf ter-f28n.pcf ter-f28b.pcf ter-f32n.pcf ter-f32b.pcf
PCF_8859_16 = ter-g12n.pcf ter-g12b.pcf ter-g14n.pcf ter-g14b.pcf ter-g16n.pcf ter-g16b.pcf ter-g18n.pcf ter-g18b.pcf ter-g20n.pcf ter-g20b.pcf ter-g22n.pcf ter-g22b.pcf ter-g24n.pcf ter-g24b.pcf ter-g28n.pcf ter-g28b.pcf ter-g32n.pcf ter-g32b.pcf
PCF_KOI8_R  = ter-k12n.pcf ter-k12b.pcf ter-k14n.pcf ter-k14b.pcf ter-k16n.pcf ter-k16b.pcf ter-k18n.pcf ter-k18b.pcf ter-k20n.pcf ter-k20b.pcf ter-k22n.pcf ter-k22b.pcf ter-k24n.pcf ter-k24b.pcf ter-k28n.pcf ter-k28b.pcf ter-k32n.pcf ter-k32b.pcf
PCF_PT_154  = ter-p12n.pcf ter-p12b.pcf ter-p14n.pcf ter-p14b.pcf ter-p16n.pcf ter-p16b.pcf ter-p18n.pcf ter-p18b.pcf ter-p20n.pcf ter-p20b.pcf ter-p22n.pcf ter-p22b.pcf ter-p24n.pcf ter-p24b.pcf ter-p28n.pcf ter-p28b.pcf ter-p32n.pcf ter-p32b.pcf
PCF_KOI8_U  = ter-u12n.pcf ter-u12b.pcf ter-u14n.pcf ter-u14b.pcf ter-u16n.pcf ter-u16b.pcf ter-u18n.pcf ter-u18b.pcf ter-u20n.pcf ter-u20b.pcf ter-u22n.pcf ter-u22b.pcf ter-u24n.pcf ter-u24b.pcf ter-u28n.pcf ter-u28b.pcf ter-u32n.pcf ter-u32b.pcf
PCF_U_10646 = ter-x12n.pcf ter-x12b.pcf ter-x14n.pcf ter-x14b.pcf ter-x16n.pcf ter-x16b.pcf ter-x18n.pcf ter-x18b.pcf ter-x20n.pcf ter-x20b.pcf ter-x22n.pcf ter-x22b.pcf ter-x24n.pcf ter-x24b.pcf ter-x28n.pcf ter-x28b.pcf ter-x32n.pcf ter-x32b.pcf
PCF = $(PCF_8859_1) $(PCF_8859_2) $(PCF_8859_5) $(PCF_8859_7) $(PCF_8859_9) $(PCF_W_1251) $(PCF_8859_13) $(PCF_8859_15) $(PCF_8859_16) $(PCF_KOI8_R) $(PCF_PT_154) $(PCF_KOI8_U) $(PCF_U_10646)

all: $(PSF) $(PCF)
psf: $(PSF)
txt: $(TXT)
raw: $(RAW)
pcf: $(PCF)
n12: $(PCF_N12)

DESTDIR	=
prefix	= /usr/local
psfdir	= $(prefix)/share/consolefonts
x11dir	= $(prefix)/share/fonts/terminus

install: $(PSF) $(PCF)
	mkdir -p $(DESTDIR)$(psfdir)
	for i in $(PSF) ; do gzip -c $$i > $(DESTDIR)$(psfdir)/$$i.gz ; done
	mkdir -p $(DESTDIR)$(x11dir)
	for i in $(PCF) ; do gzip -c $$i > $(DESTDIR)$(x11dir)/$$i.gz ; done

uninstall:
	for i in $(PSF) ; do rm -f $(DESTDIR)$(psfdir)/$$i.gz ; done
	for i in $(PCF) ; do rm -f $(DESTDIR)$(x11dir)/$$i.gz ; done

fontdir:
	mkfontscale $(DESTDIR)$(x11dir)
	mkfontdir $(DESTDIR)$(x11dir)
	fc-cache -f $(DESTDIR)$(x11dir)

VGA_8859_1  = uni/vgagr.uni uni/ascii-h.uni uni/win-1252.uni
VGA_8859_2  = uni/vgagr.uni uni/ascii-h.uni uni/vga-1250.uni uni/8859-2.uni
VGA_8859_7  = uni/vgagr.uni uni/ascii-h.uni uni/vga-1253.uni uni/8859-7.uni
VGA_8859_9  = uni/vgagr.uni uni/ascii-h.uni uni/win-1254.uni
VGA_W_1251  = uni/vgagr.uni uni/ascii-h.uni uni/vga-1251.uni uni/win-1251.uni
VGA_8859_13 = uni/vgagr.uni uni/ascii-h.uni uni/vga-1257.uni uni/8859-13.uni
VGA_8859_16 = uni/vgagr.uni uni/ascii-h.uni uni/nls-1250.uni uni/8859-16.uni
VGA_IBM_437 = uni/cntrl.uni uni/ascii-h.uni uni/ibm-437.uni
VGA_KOI8_RV = uni/cntrl.uni uni/ascii-h.uni uni/koibm8-r.uni
VGA_KOI8_R  = uni/cntrl.uni uni/ascii-h.uni uni/koi8-r.uni
VGA_BG_MIK  = uni/cntrl.uni uni/ascii-h.uni uni/bg-mik.uni
VGA_PT_154  = uni/vgagr.uni uni/ascii-h.uni uni/pt-154.uni
VGA_KOI8_UV = uni/cntrl.uni uni/ascii-h.uni uni/koibm8-u.uni
VGA_KOI8_U  = uni/cntrl.uni uni/ascii-h.uni uni/koi8-u.uni
VGA_XOS4_2  = uni/xos4-2.uni

DUP_8859_1  = dup/vgagr.dup dup/ascii-h.dup
DUP_8859_2  = dup/vgagr.dup dup/ascii-h.dup
DUP_8859_7  = dup/vgagr.dup dup/ascii-h.dup
DUP_8859_9  = dup/vgagr.dup dup/ascii-h.dup
DUP_W_1251  = dup/vgagr.dup dup/ascii-h.dup
DUP_8859_13 = dup/vgagr.dup dup/ascii-h.dup
DUP_8859_16 = dup/vgagr.dup dup/ascii-h.dup
DUP_IBM_437 = dup/cntrl.dup dup/ascii-h.dup dup/ibm-437.dup
DUP_KOI8_RV = dup/cntrl.dup dup/ascii-h.dup dup/koi8.dup
DUP_KOI8_R  = dup/cntrl.dup dup/ascii-h.dup dup/koi8.dup
DUP_BG_MIK  = dup/cntrl.dup dup/ascii-h.dup dup/ibm-437.dup
DUP_PT_154  = dup/vgagr.dup dup/ascii-h.dup
DUP_KOI8_UV = dup/cntrl.dup dup/ascii-h.dup dup/koi8.dup
DUP_KOI8_U  = dup/cntrl.dup dup/ascii-h.dup dup/koi8.dup
DUP_XOS4_2  = dup/vgagr.dup dup/xos4-2.dup

$(PSF_8859_1): ter-1%.psf : ter-u%.bdf $(VGA_8859_1) $(DUP_8859_1)
	$(UCS2VGA) $< $(REG_8859_1) $(VGA_8859_1) | $(BDF2PSF) -o $@ - $(DUP_8859_1)

$(PSF_8859_2): ter-2%.psf : ter-u%.bdf $(VGA_8859_2) $(DUP_8859_2)
	$(UCS2VGA) $< $(REG_8859_2) $(VGA_8859_2) | $(BDF2PSF) -o $@ - $(DUP_8859_2)

$(PSF_8859_5): ter-5%.psf : ter-u%.bdf $(VGA_8859_5) $(DUP_8859_5)
	$(UCS2VGA) $< $(REG_8859_5) $(VGA_8859_5) | $(BDF2PSF) -o $@ - $(DUP_8859_5)

$(PSF_8859_7): ter-7%.psf : ter-u%.bdf $(VGA_8859_7) $(DUP_8859_7)
	$(UCS2VGA) $< $(REG_8859_7) $(VGA_8859_7) | $(BDF2PSF) -o $@ - $(DUP_8859_7)

$(PSF_8859_9): ter-9%.psf : ter-u%.bdf $(VGA_8859_9) $(DUP_8859_9)
	$(UCS2VGA) $< $(REG_8859_9) $(VGA_8859_9) | $(BDF2PSF) -o $@ - $(DUP_8859_9)

$(PSF_W_1251): ter-c%.psf : ter-u%.bdf $(VGA_W_1251) $(DUP_W_1251)
	$(UCS2VGA) $< $(REG_W_1251) $(VGA_W_1251) | $(BDF2PSF) -o $@ - $(DUP_W_1251)

$(PSF_8859_13): ter-d%.psf : ter-u%.bdf $(VGA_8859_13) $(DUP_8859_13)
	$(UCS2VGA) $< $(REG_8859_13) $(VGA_8859_13) | $(BDF2PSF) -o $@ - $(DUP_8859_13)

$(PSF_8859_16): ter-g%.psf : ter-u%.bdf $(VGA_8859_16) $(DUP_8859_16)
	$(UCS2VGA) $< $(REG_8859_16) $(VGA_8859_16) | $(BDF2PSF) -o $@ - $(DUP_8859_16)

$(PSF_IBM_437): ter-i%.psf : ter-u%.bdf $(VGA_IBM_437) $(DUP_IBM_437)
	$(UCS2VGA) $< $(REG_IBM_437) $(VGA_IBM_437) | $(BDF2PSF) -o $@ - $(DUP_IBM_437)

$(PSF_KOI8_RV): ter-k%.psf : ter-u%.bdf $(VGA_KOI8_RV) $(DUP_KOI8_RV)
	$(UCS2VGA) $< $(REG_KOI8_R) $(VGA_KOI8_RV) | $(BDF2PSF) -o $@ - $(DUP_KOI8_RV)

$(PSF_KOI8_R): ter-k%.psf : ter-u%.bdf $(VGA_KOI8_R) $(DUP_KOI8_R)
	$(UCS2VGA) $< $(REG_KOI8_R) $(VGA_KOI8_R) | $(BDF2PSF) -o $@ - $(DUP_KOI8_R)

$(PSF_BG_MIK): ter-m%.psf : ter-u%.bdf $(VGA_BG_MIK) $(DUP_BG_MIK)
	$(UCS2VGA) $< $(REG_BG_MIK) $(VGA_BG_MIK) | $(BDF2PSF) -o $@ - $(DUP_BG_MIK)

$(PSF_PT_154): ter-p%.psf : ter-u%.bdf $(VGA_PT_154) $(DUP_PT_154)
	$(UCS2VGA) $< $(REG_PT_154) $(VGA_PT_154) | $(BDF2PSF) -o $@ - $(DUP_PT_154)

$(PSF_KOI8_UV): ter-u%.psf : ter-u%.bdf $(VGA_KOI8_UV) $(DUP_KOI8_UV)
	$(UCS2VGA) $< $(REG_KOI8_R) $(VGA_KOI8_UV) | $(BDF2PSF) -o $@ - $(DUP_KOI8_UV)

$(PSF_KOI8_U): ter-u%.psf : ter-u%.bdf $(VGA_KOI8_U) $(DUP_KOI8_U)
	$(UCS2VGA) $< $(REG_KOI8_U) $(VGA_KOI8_U) | $(BDF2PSF) -o $@ - $(DUP_KOI8_U)

$(PSF_XOS4_2): ter-v%.psf : ter-u%.bdf $(VGA_XOS4_2) $(DUP_XOS4_2)
	$(UCS2VGA) $< $(REG_XOS4_2) $(VGA_XOS4_2) | $(BDF2PSF) -o $@ - $(DUP_XOS4_2)

install-psf: $(PSF)
	mkdir -p $(DESTDIR)$(psfdir)
	for i in $(PSF) ; do gzip -c $$i > $(DESTDIR)$(psfdir)/$$i.gz ; done

uninstall-psf:
	for i in $(PSF) ; do rm -f $(DESTDIR)$(psfdir)/$$i.gz ; done

install-psf-512: $(PSF_XOS4_2)
	mkdir -p $(DESTDIR)$(psfdir)
	for i in $(PSF_XOS4_2) ; do gzip -c $$i > $(DESTDIR)$(psfdir)/$$i.gz ; done

uninstall-psf-512:
	for i in $(PSF_XOS4_2) ; do rm -f $(DESTDIR)$(psfdir)/$$i.gz ; done

ref = $(psfdir)/README.terminus

install-ref: README
	mkdir -p $(DESTDIR)$(psfdir)
	sed -e"/^2\.4/,/^2\.5/p" -n README | grep -v "^2\." > $(DESTDIR)$(ref)

uninstall-ref:
	rm -f $(DESTDIR)$(ref)

TXT_W_1252  = uni/cntrl.uni uni/ascii-h.uni uni/win-1252.uni
TXT_W_1250  = uni/cntrl.uni uni/ascii-h.uni uni/win-1250.uni
TXT_W_1253  = uni/cntrl.uni uni/ascii-h.uni uni/win-1253.uni
TXT_W_1254  = uni/cntrl.uni uni/ascii-h.uni uni/win-1254.uni
TXT_W_1251  = uni/cntrl.uni uni/ascii-h.uni uni/x11-1251.uni uni/win-1251.uni
TXT_W_1257  = uni/cntrl.uni uni/ascii-h.uni uni/x11-1257.uni uni/win-1257.uni
TXT_8859_16 = uni/cntrl.uni uni/ascii-h.uni uni/empty.uni uni/8859-16.uni
TXT_IBM_437 = uni/cntrl.uni uni/ascii-h.uni uni/ibm-437.uni
TXT_KOI8_R  = uni/cntrl.uni uni/ascii-h.uni uni/koi8-r.uni
TXT_KOI8_U  = uni/cntrl.uni uni/ascii-h.uni uni/koi8-u.uni
TXT_BG_MIK  = uni/cntrl.uni uni/ascii-h.uni uni/bg-mik.uni
TXT_PT_154  = uni/cntrl.uni uni/ascii-h.uni uni/pt-154.uni
TXT_PT_254  = uni/cntrl.uni uni/ascii-h.uni uni/pt-254.uni

cp1252.txt: $(TXT_W_1252)
	cat $(TXT_W_1252) | $(UNI2TXT) > $@

cp1250.txt: $(TXT_W_1250)
	cat $(TXT_W_1250) | $(UNI2TXT) > $@

cp1253.txt: $(TXT_W_1253)
	cat $(TXT_W_1253) | $(UNI2TXT) > $@

cp1254.txt: $(TXT_W_1254)
	cat $(TXT_W_1254) | $(UNI2TXT) > $@

cp1251.txt: $(TXT_W_1251)
	cat $(TXT_W_1251) | $(UNI2TXT) > $@

cp1257.txt: $(TXT_W_1257)
	cat $(TXT_W_1257) | $(UNI2TXT) > $@

8859-16.txt: $(TXT_8859_16)
	cat $(TXT_8859_16) | $(UNI2TXT) > $@

cp437.txt: $(TXT_IBM_437)
	cat $(TXT_IBM_437) | $(UNI2TXT) > $@

koi8-r.txt: $(TXT_KOI8_R)
	cat $(TXT_KOI8_R) | $(UNI2TXT) > $@

koi8-u.txt: $(TXT_KOI8_U)
	cat $(TXT_KOI8_U) | $(UNI2TXT) > $@

mik.txt: $(TXT_BG_MIK)
	cat $(TXT_BG_MIK) | $(UNI2TXT) > $@

pt154.txt: $(TXT_PT_154)
	cat $(TXT_PT_154) | $(UNI2TXT) > $@

pt254.txt: $(TXT_PT_254)
	cat $(TXT_PT_254) | $(UNI2TXT) > $@

CHECKDIR =

acmdir = $(prefix)/share/consoletrans

install-acm: $(TXT)
	mkdir -p $(DESTDIR)$(acmdir)
	for i in $(TXT) ; do \
		a=`echo $$i | sed -e "s/\.txt$$/.acm.gz/"` ; \
		if test ! -f $(CHECKDIR)$(acmdir)/$$a ; then \
			tail -n 128 $$i | grep -v FFFF | gzip > $(DESTDIR)$(acmdir)/$$a ; \
		fi ; \
	done

unidir = $(prefix)/share/kbd/consoletrans

install-uni: $(TXT)
	mkdir -p $(DESTDIR)$(unidir)
	for i in $(TXT) ; do \
		u=`echo $$i | sed -e "s/\.txt$$/_to_uni.trans/"` ; \
		if test ! -f $(CHECKDIR)$(unidir)/$$u ; then \
			sed -e "s/FFFF/FFFD/" $$i > $(DESTDIR)$(unidir)/$$u ; \
		fi ; \
	done

BIN_8859_1  = uni/cntrl.uni uni/ascii-h.uni uni/win-1252.uni
BIN_8859_2  = uni/cntrl.uni uni/ascii-h.uni uni/empty.uni uni/8859-2.uni
BIN_8859_5  = uni/cntrl.uni uni/ascii-h.uni uni/empty.uni uni/8859-5.uni
BIN_8859_7  = uni/cntrl.uni uni/ascii-h.uni uni/empty.uni uni/8859-7.uni
BIN_8859_9  = uni/cntrl.uni uni/ascii-h.uni uni/win-1254.uni
BIN_W_1251  = uni/cntrl.uni uni/ascii-h.uni uni/x11-1251.uni uni/win-1251.uni
BIN_8859_13 = uni/cntrl.uni uni/ascii-h.uni uni/x11-1257.uni uni/8859-13.uni
BIN_8859_15 = uni/cntrl.uni uni/ascii-h.uni uni/empty.uni uni/8859-15.uni
BIN_8859_16 = uni/cntrl.uni uni/ascii-h.uni uni/empty.uni uni/8859-16.uni
BIN_IBM_437 = uni/cntrl.uni uni/ascii-h.uni uni/ibm-437.uni
BIN_KOI8_R  = uni/cntrl.uni uni/ascii-h.uni uni/koi8-r.uni
BIN_PT_154  = uni/cntrl.uni uni/ascii-h.uni uni/pt-154.uni
BIN_KOI8_U  = uni/cntrl.uni uni/ascii-h.uni uni/koi8-u.uni

$(RAW_8859_1): ter-1%.raw : ter-u%.bdf $(BIN_8859_1)
	$(UCS2VGA) $< $(REG_8859_1) $(BIN_8859_1) | $(BDF2RAW) -o $@

$(RAW_8859_2): ter-2%.raw : ter-u%.bdf $(BIN_8859_2)
	$(UCS2VGA) $< $(REG_8859_2) $(BIN_8859_2) | $(BDF2RAW) -o $@

$(RAW_8859_5): ter-5%.raw : ter-u%.bdf $(BIN_8859_5)
	$(UCS2VGA) $< $(REG_8859_5) $(BIN_8859_5) | $(BDF2RAW) -o $@

$(RAW_8859_7): ter-7%.raw : ter-u%.bdf $(BIN_8859_7)
	$(UCS2VGA) $< $(REG_8859_7) $(BIN_8859_7) | $(BDF2RAW) -o $@

$(RAW_8859_9): ter-9%.raw : ter-u%.bdf $(BIN_8859_9)
	$(UCS2VGA) $< $(REG_8859_9) $(BIN_8859_9) | $(BDF2RAW) -o $@

$(RAW_W_1251): ter-c%.raw : ter-u%.bdf $(BIN_W_1251)
	$(UCS2VGA) $< $(REG_W_1251) $(BIN_W_1251) | $(BDF2RAW) -o $@

$(RAW_8859_13): ter-d%.raw : ter-u%.bdf $(BIN_8859_13)
	$(UCS2VGA) $< $(REG_8859_13) $(BIN_8859_13) | $(BDF2RAW) -o $@

$(RAW_8859_15): ter-f%.raw : ter-u%.bdf $(BIN_8859_15)
	$(UCS2VGA) $< $(REG_8859_15) $(BIN_8859_15) | $(BDF2RAW) -o $@

$(RAW_8859_16): ter-g%.raw : ter-u%.bdf $(BIN_8859_16)
	$(UCS2VGA) $< $(REG_8859_16) $(BIN_8859_16) | $(BDF2RAW) -o $@

$(RAW_IBM_437): ter-i%.raw : ter-u%.bdf $(BIN_IBM_437)
	$(UCS2VGA) $< $(REG_IBM_437) $(BIN_IBM_437) | $(BDF2RAW) -o $@

$(RAW_KOI8_R): ter-k%.raw : ter-u%.bdf $(BIN_KOI8_R)
	$(UCS2VGA) $< $(REG_KOI8_R) $(BIN_KOI8_R) | $(BDF2RAW) -o $@

$(RAW_PT_154): ter-p%.raw : ter-u%.bdf $(BIN_PT_154)
	$(UCS2VGA) $< $(REG_PT_154) $(BIN_PT_154) | $(BDF2RAW) -o $@

$(RAW_KOI8_U): ter-u%.raw : ter-u%.bdf $(BIN_KOI8_U)
	$(UCS2VGA) $< $(REG_KOI8_U) $(BIN_KOI8_U) | $(BDF2RAW) -o $@

rawdir = $(prefix)/share/misc/pcvtfonts

RAW_14 = $(RAW_14N) $(RAW_14B) $(RAW_14V)
RAW_16 = $(RAW_16N) $(RAW_16B) $(RAW_16V)

point1x	= echo $$i | sed -e "s/1\([46]\)\([nbv]\)\.raw/\2.81\1/"

install.raw: $(RAW)
	mkdir -p $(DESTDIR)$(rawdir)
	for i in $(RAW) ; do cp -f $$i $(DESTDIR)$(rawdir)/`$(point1x)` ; done

uninstall.raw:
	for i in $(RAW) ; do rm -f $(DESTDIR)$(rawdir)/`$(point1x)` ; done

minus1x	= echo $$i | sed -e "s/1\([46]\)\([nbv]\)\.raw/\2-8x1\1/"

install-raw: $(RAW)
	mkdir -p $(DESTDIR)$(rawdir)
	for i in $(RAW) ; do cp -f $$i $(DESTDIR)$(rawdir)/`$(minus1x)` ; done

uninstall-raw:
	for i in $(RAW) ; do rm -f $(DESTDIR)$(rawdir)/`$(minus1x)` ; done

X11_8859_1  = uni/x11gr.uni uni/ascii-h.uni uni/win-1252.uni
X11_8859_2  = uni/x11gr.uni uni/ascii-h.uni uni/empty.uni uni/8859-2.uni
X11_8859_5  = uni/x11gr.uni uni/ascii-h.uni uni/empty.uni uni/8859-5.uni
X11_8859_7  = uni/x11gr.uni uni/ascii-h.uni uni/empty.uni uni/8859-7.uni
X11_8859_9  = uni/x11gr.uni uni/ascii-h.uni uni/win-1254.uni
X11_W_1251  = uni/x11gr.uni uni/ascii-h.uni uni/x11-1251.uni uni/win-1251.uni
X11_8859_13 = uni/x11gr.uni uni/ascii-h.uni uni/x11-1257.uni uni/8859-13.uni
X11_8859_15 = uni/x11gr.uni uni/ascii-h.uni uni/empty.uni uni/8859-15.uni
X11_8859_16 = uni/x11gr.uni uni/ascii-h.uni uni/empty.uni uni/8859-16.uni
X11_KOI8_R  = uni/x11gr.uni uni/ascii-h.uni uni/koi8-r.uni
X11_PT_154  = uni/x11gr.uni uni/ascii-h.uni uni/pt-154.uni
X11_KOI8_U  = uni/x11gr.uni uni/ascii-h.uni uni/koi8-u.uni
X11_U_10646 = uni/x11gr.uni uni/10646-1.uni

$(PCF_8859_1): ter-1%.pcf : ter-u%.bdf $(X11_8859_1)
	$(UCS2X11) $< $(REG_8859_1) $(X11_8859_1) | $(BDF2PCF) -o $@

$(PCF_8859_2): ter-2%.pcf : ter-u%.bdf $(X11_8859_2)
	$(UCS2X11) $< $(REG_8859_2) $(X11_8859_2) | $(BDF2PCF) -o $@

$(PCF_8859_5): ter-5%.pcf : ter-u%.bdf $(X11_8859_5)
	$(UCS2X11) $< $(REG_8859_5) $(X11_8859_5) | $(BDF2PCF) -o $@

$(PCF_8859_7): ter-7%.pcf : ter-u%.bdf $(X11_8859_7)
	$(UCS2X11) $< $(REG_8859_7) $(X11_8859_7) | $(BDF2PCF) -o $@

$(PCF_8859_9): ter-9%.pcf : ter-u%.bdf $(X11_8859_9)
	$(UCS2X11) $< $(REG_8859_9) $(X11_8859_9) | $(BDF2PCF) -o $@

$(PCF_W_1251): ter-c%.pcf : ter-u%.bdf $(X11_W_1251)
	$(UCS2X11) $< $(REG_W_1251) $(X11_W_1251) | $(BDF2PCF) -o $@

$(PCF_8859_13): ter-d%.pcf : ter-u%.bdf $(X11_8859_13)
	$(UCS2X11) $< $(REG_8859_13) $(X11_8859_13) | $(BDF2PCF) -o $@

$(PCF_8859_15): ter-f%.pcf : ter-u%.bdf $(X11_8859_15)
	$(UCS2X11) $< $(REG_8859_15) $(X11_8859_15) | $(BDF2PCF) -o $@

$(PCF_8859_16): ter-g%.pcf : ter-u%.bdf $(X11_8859_16)
	$(UCS2X11) $< $(REG_8859_16) $(X11_8859_16) | $(BDF2PCF) -o $@

$(PCF_KOI8_R): ter-k%.pcf : ter-u%.bdf $(X11_KOI8_R)
	$(UCS2X11) $< $(REG_KOI8_R) $(X11_KOI8_R) | $(BDF2PCF) -o $@

$(PCF_PT_154): ter-p%.pcf : ter-u%.bdf $(X11_PT_154)
	$(UCS2X11) $< $(REG_PT_154) $(X11_PT_154) | $(BDF2PCF) -o $@

$(PCF_KOI8_U): ter-u%.pcf : ter-u%.bdf $(X11_KOI8_U)
	$(UCS2X11) $< $(REG_KOI8_U) $(X11_KOI8_U) | $(BDF2PCF) -o $@

$(PCF_U_10646): ter-x%.pcf : ter-u%.bdf $(X11_U_10646)
	$(UCS2X11) $< $(REG_U_10646) $(X11_U_10646) | $(BDF2PCF) -o $@

install-pcf: $(PCF)
	mkdir -p $(DESTDIR)$(x11dir)
	for i in $(PCF) ; do gzip -c $$i > $(DESTDIR)$(x11dir)/$$i.gz ; done

uninstall-pcf:
	for i in $(PCF) ; do rm -f $(DESTDIR)$(x11dir)/$$i.gz ; done

install-pcf-646: $(PCF_U_10646)
	mkdir -p $(DESTDIR)$(x11dir)
	for i in $(PCF_U_10646) ; do gzip -c $$i > $(DESTDIR)$(x11dir)/$$i.gz ; done

uninstall-pcf-646:
	for i in $(PCF_U_10646) ; do rm -f $(DESTDIR)$(x11dir)/$$i.gz ; done

cut12b_ = sed -e "s/ter-.12b\.pcf //g"

install-n12: $(PCF)
	mkdir -p $(DESTDIR)$(x11dir)
	for i in `echo $(PCF) | $(cut12b_)` ; do ; gzip -c $$i > $(DESTDIR)$(x11dir)/$$i.gz ; done

uninstall-n12:
	for i in `echo $(PCF) | $(cut12b_)` ; do rm -f $(DESTDIR)$(x11dir)/$$i.gz ; done

install-n12-646: $(PCF_U_10646)
	mkdir -p $(DESTDIR)$(x11dir)
	for i in `echo $(PCF_U_10646) | $(cut12b_)` ; do gzip -c $$i > $(DESTDIR)$(x11dir)/$$i.gz ; done

uninstall-n12-646:
	for i in `echo $(PCF_U_10646) | $(cut12b_)` ; do rm -f $(DESTDIR)$(x11dir)/$$i.gz ; done

clean:
	rm -f *.psf *.raw *.pcf
	rm -f $(TXT)
