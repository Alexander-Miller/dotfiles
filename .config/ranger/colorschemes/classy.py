from ranger.gui.colorscheme import ColorScheme
from ranger.gui.color import *

class Default(ColorScheme):
    def use(self, context):
        fg, bg, attr = default_colors

        if context.reset:
            return default_colors

        elif context.in_browser:
            if context.empty or context.error:
                fg = 85 #grey
            if context.border:
                attr = normal
                fg = 12 #brown-yellow
            if context.media:
                fg = 20 #dark green
            if context.container:
                attr |= normal
                fg = 54 #pale purple
            if context.directory:
                attr |= normal
                fg = blue
            elif context.executable and not \
                    any((context.media, context.container,
                        context.fifo, context.socket)):
                attr |= normal
                fg = 26
            if context.socket:
                fg = cyan
            if context.fifo or context.device:
                fg = yellow
                if context.device:
                    attr |= bold
            if context.link:
                fg = white
            if context.tag_marker and not context.selected:
                attr |= bold
                if fg in (red, white):
                    fg = black
                else:
                    fg = green
            if not context.selected and (context.cut or context.copied):
                fg = white
                attr |= bold
            if context.main_column:
                if context.selected:
                    fg = red
                    attr |= normal
                if context.marked:
                    attr |= underline
                    if not context.selected:
                        fg = yellow
            if context.badinfo:
                if attr & reverse:
                    bg = red.co
                else:
                    fg = red

            if context.selected:
                fg = red
                attr = bold
            else:
                attr = normal

        elif context.in_titlebar:
            attr |= normal
            if context.hostname:
                attr |= normal
                fg = red
            elif context.directory:
                fg = red
            elif context.tab:
                if context.good:
                    bg = green
            elif context.link:
                fg = magenta

        elif context.in_statusbar:
            if context.permissions:
                if context.good:
                    fg = black
                elif context.bad:
                    fg = red
            if context.marked:
                attr |= bold | reverse
                fg = yellow
            if context.message:
                if context.bad:
                    attr |= bold
                    fg = red

        if context.text:
            if context.highlight:
                attr |= bold

        if context.in_taskview:
            if context.title:
                fg = red

            if context.selected:
                attr |= normal

        return fg, bg, attr
