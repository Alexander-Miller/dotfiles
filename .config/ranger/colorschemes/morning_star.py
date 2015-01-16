from ranger.gui.colorscheme import ColorScheme
from ranger.gui.color import *

class Default(ColorScheme):
    def use(self, context):
        fg   = 0
        attr = default_colors[2]
        bg   = -1

        if context.reset:
            return default_colors

        elif context.in_browser:
            if context.fifo or context.socket:
                fg = 21
            if context.device:
                bg = 77
            if context.badinfo:
                fg = 64
            if context.error:
                fg = 64
            if context.container:
                fg = 13
            if context.border:
                fg = 7
            if context.media:
                fg = 41
            if context.executable:
                fg = 77
            if context.empty:
                fg = 83
            if context.directory:
                fg = 12
            if context.selected:
                bg = fg
                fg = 16
            elif context.device:
                fg = 12
                
        elif context.in_titlebar:
            attr |= bold
            if context.hostname:
                fg = context.bad and red or green
            elif context.directory:
                fg = blue
            elif context.tab:
                if context.good:
                    fg = red
            elif context.link:
                fg = cyan

        elif context.in_statusbar:
            if context.permissions:
                if context.good:
                    fg = cyan
                elif context.bad:
                    fg = magenta
            if context.marked:
                attr |= bold | reverse
                fg = yellow
            if context.message:
                if context.bad:
                    attr |= bold
                    fg = red
            if context.vcsinfo:
                fg = blue
                attr &= ~bold
            if context.vcscommit:
                fg = yellow
                attr &= ~bold
        
        return fg, bg, attr

