from ranger.gui.colorscheme import ColorScheme
from ranger.gui.color import *

class Default(ColorScheme):
    def use(self, context):
        fg   = 58
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
#            elif context.device:
#                fg = 12
#            if context.tag_marker and not context.selected:
#            if not context.selected and (context.cut or context.copied):
#        elif context.in_titlebar:
#            #attr |= normal
#            if context.hostname:
#             #   attr |= normal
#              #  fg = red
#            elif context.directory:
#               # fg = red
#            elif context.tab:
#                if context.good:
#                #    bg = green
#            elif context.link:
#                #fg = magenta
#
#        elif context.in_statusbar:
#            if context.permissions:
#                if context.good:
#                 #   fg = white
#                elif context.bad:
#                  #  fg = red
#            if context.marked:
#                #attr |= bold | reverse
#                #fg = yellow
#            if context.message:
#                if context.bad:
#                 #   attr |= bold
#                  #  fg = red
#
#        if context.text:
#            if context.highlight:
#                #attr |= bold
#
#        if context.in_taskview:
#            if context.title:
#                #fg = red
#
#            if context.selected:
#                #attr |= normal
        
        return fg, bg, attr
