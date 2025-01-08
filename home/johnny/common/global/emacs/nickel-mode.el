(require 'nickel-mode)
(require 'lsp-mode)

(add-to-list 'lsp-language-id-configuration '(nickel-mode . "nickel"))
(lsp-register-client (make-lsp-client
                       :new-connection (lsp-stdio-connection "nls")
			                 :activation-fn (lsp-activate-on "nickel")
                       :server-id 'nls
                       :major-modes 'nickel-mode))
(add-hook 'nickel-mode-hook 'lsp-deferred)
