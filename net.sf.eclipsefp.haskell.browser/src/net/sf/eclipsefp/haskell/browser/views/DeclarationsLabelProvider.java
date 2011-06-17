package net.sf.eclipsefp.haskell.browser.views;

import net.sf.eclipsefp.haskell.browser.items.Constructor;
import net.sf.eclipsefp.haskell.browser.items.Declaration;
import net.sf.eclipsefp.haskell.browser.items.Function;
import net.sf.eclipsefp.haskell.browser.items.Gadt;
import net.sf.eclipsefp.haskell.browser.items.Instance;
import net.sf.eclipsefp.haskell.browser.items.Packaged;
import net.sf.eclipsefp.haskell.browser.items.TypeClass;
import net.sf.eclipsefp.haskell.browser.items.TypeSynonym;
import net.sf.eclipsefp.haskell.browser.util.ImageCache;

import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.swt.graphics.Image;

public class DeclarationsLabelProvider implements ILabelProvider {

	public Image getImage(Object element) {
		if (element instanceof Packaged<?>) {
			Packaged<Declaration> pDecl = (Packaged<Declaration>) element;
			Declaration decl = pDecl.getElement();
			switch (decl.getType()) {
			case DATA_TYPE:
			case NEW_TYPE:
				return ImageCache.DATATYPE;
			case TYPE_CLASS:
				return ImageCache.CLASS;
			case INSTANCE:
				return ImageCache.INSTANCE;
			case FUNCTION:
				return ImageCache.FUNCTION;
			case TYPE_SYNONYM:
				return ImageCache.TYPE;
			}
		} else if (element instanceof Constructor) {
			return ImageCache.CONSTRUCTOR;
		}

		return null;
	}

	public String getText(Object element) {
		if (element instanceof Packaged<?>) {
			Declaration elt = ((Packaged<Declaration>) element).getElement();
			if (elt instanceof Gadt) {
				Gadt item = (Gadt) elt;
				StringBuilder name = new StringBuilder(item.getName());
				for (String var : item.getTypeVariables()) {
					name.append(' ');
					name.append(var);
				}
				return name.toString();
			} else if (elt instanceof TypeClass) {
				TypeClass item = (TypeClass) elt;
				StringBuilder name = new StringBuilder(item.getName());
				for (String var : item.getTypeVariables()) {
					name.append(' ');
					name.append(var);
				}
				return name.toString();
			} else if (elt instanceof Instance) {
				Instance item = (Instance) elt;
				StringBuilder name = new StringBuilder();
				for (String ctx : item.getContext()) {
					name.append(ctx);
					name.append(' ');
				}
				if (item.getContext().length > 0)
					name.append("=> ");
				name.append(item.getName());
				for (String var : item.getTypeVariables()) {
					name.append(' ');
					name.append(var);
				}
				return name.toString();
			} else if (elt instanceof TypeSynonym) {
				TypeSynonym item = (TypeSynonym) elt;
				StringBuilder name = new StringBuilder(item.getName());
				for (String var : item.getTypeVariables()) {
					name.append(' ');
					name.append(var);
				}
				name.append(" = ");
				name.append(item.getEquivalence());
				return name.toString();
			} else if (elt instanceof Function) {
				Function item = (Function) elt;
				return item.getName() + " :: " + item.getSignature();
			}
		} else if (element instanceof Constructor) {
			Constructor item = (Constructor) element;
			return item.getName() + " :: " + item.getSignature();
		}

		return null;
	}

	// Listeners: not used
	public void addListener(ILabelProviderListener listener) {
		// Do nothing
	}

	public void dispose() {
		// Do nothing
	}

	public boolean isLabelProperty(Object element, String property) {
		// Do nothing
		return false;
	}

	public void removeListener(ILabelProviderListener listener) {
		// Do nothing
	}

}
