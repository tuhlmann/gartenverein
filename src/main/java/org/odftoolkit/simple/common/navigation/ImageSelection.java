/*
Licensed to the Apache Software Foundation (ASF) under one
or more contributor license agreements.  See the NOTICE file
distributed with this work for additional information
regarding copyright ownership.  The ASF licenses this file
to you under the Apache License, Version 2.0 (the
"License"); you may not use this file except in compliance
with the License.  You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing,
software distributed under the License is distributed on an
"AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
KIND, either express or implied.  See the License for the
specific language governing permissions and limitations
under the License.
*/

package org.odftoolkit.simple.common.navigation;

import java.awt.image.BufferedImage;
import java.io.InputStream;
import java.lang.reflect.Method;
import java.net.URI;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.imageio.ImageIO;

import org.odftoolkit.odfdom.dom.OdfDocumentNamespace;
import org.odftoolkit.odfdom.dom.OdfSchemaDocument;
import org.odftoolkit.odfdom.dom.element.draw.DrawFrameElement;
import org.odftoolkit.odfdom.dom.element.draw.DrawImageElement;
import org.odftoolkit.odfdom.dom.element.text.TextSElement;
import org.odftoolkit.odfdom.pkg.OdfElement;
import org.odftoolkit.odfdom.pkg.OdfFileDom;
import org.odftoolkit.odfdom.pkg.OdfPackage;
import org.odftoolkit.odfdom.pkg.manifest.OdfFileEntry;
import org.odftoolkit.odfdom.type.AnyURI;
import org.odftoolkit.odfdom.type.Length;
import org.odftoolkit.odfdom.type.Length.Unit;
import org.odftoolkit.simple.common.TextExtractor;
import org.odftoolkit.simple.draw.Frame;
import org.odftoolkit.simple.draw.FrameRectangle;
import org.odftoolkit.simple.draw.Image;
import org.odftoolkit.simple.style.StyleTypeDefinitions;
import org.odftoolkit.simple.style.StyleTypeDefinitions.AnchorType;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * This is a decorator class of TextSelection, which help user replace a text content with a Image from Document or from URI.
 *
 */
public class ImageSelection extends Selection {

	private static final String SLASH = "/";

	private TextSelection textSelection;
	private DrawFrameElement imageContainer;

	private boolean mIsInserted;

	/**
	 * Replace the content with a Image
	 *
	 * @param imageUri the URI of the reference Image to replace
	 *
	 * @return the new Image in the TextDocument,the image name is set to "replace" + System.currentTimeMillis(), please update the name to others by yourself
	 *
	 */
	public Image replaceWithImage(URI imageUri) {
		Image mImage =null;
		int leftLength = textSelection.getText().length();
		int index = textSelection.getIndex();
		mIsInserted = false;
		OdfElement parentElement = textSelection.getContainerElement();
		OdfFileDom ownerDom = (OdfFileDom) parentElement.getOwnerDocument();
		try{
			if (imageContainer == null) {
				delete(index, leftLength, parentElement);
				// PrepareContainer
				imageContainer = ownerDom.newOdfElement(DrawFrameElement.class);
				insertOdfElement(imageContainer, index, parentElement);
			} else {
				NodeList nodeImages = imageContainer.getElementsByTagName("draw:image");
				Node nodeImage = nodeImages.item(0);
				DrawImageElement im = (DrawImageElement) nodeImage;
				Image oldimage = Image.getInstanceof(im);
				oldimage.remove();
				// PrepareContainer
				imageContainer = ownerDom.newOdfElement(DrawFrameElement.class);
				insertOdfElement(imageContainer, index, parentElement);
			}
			// Insert Image resource to package
			DrawImageElement imageElement = imageContainer.newDrawImageElement();
			String imageRef = imageUri.toString();
			String mediaType = OdfFileEntry.getMediaTypeString(imageRef);
			OdfSchemaDocument mOdfSchemaDoc = (OdfSchemaDocument) ownerDom.getDocument();
			String packagePath = getPackagePath(mOdfSchemaDoc, imageRef);
			mOdfSchemaDoc.getPackage().insert(imageUri, packagePath, mediaType);
			packagePath = packagePath.replaceFirst(ownerDom.getDocument().getDocumentPath(), "");
			configureInsertedImage((OdfSchemaDocument) ownerDom.getDocument(), imageElement,packagePath, false);
			// get image object
			mImage = Image.getInstanceof(imageElement);
			mImage.getStyleHandler().setAchorType(AnchorType.AS_CHARACTER);
			mImage.setName("replace" + System.currentTimeMillis());

		} catch (Exception e) {
			Logger.getLogger(ImageSelection.class.getName()).log(Level.SEVERE, e.getMessage(), e);
		}

		return mImage;
	}

	/* Helper method */
	private static String getPackagePath(OdfSchemaDocument mOdfSchemaDoc, String imageRef) {
		if (imageRef.contains(SLASH)) {
			imageRef = imageRef.substring(imageRef.lastIndexOf(SLASH) + 1, imageRef.length());
		}
		String packagePath = OdfPackage.OdfFile.IMAGE_DIRECTORY.getPath() + SLASH + imageRef;
		return packagePath = mOdfSchemaDoc.getDocumentPath() + packagePath;
	}


	/* Helper method */
	private static URI configureInsertedImage(OdfSchemaDocument mOdfSchemaDoc, DrawImageElement imageElement,
			String packagePath, boolean isResetSize) throws Exception {
		// Set path to image attribute
		URI uri = new URI(AnyURI.encodePath(packagePath).toString());
		imageElement.setXlinkHrefAttribute(AnyURI.decodePath(uri.toString()));
		// Set mandatory attribute xlink:type
		imageElement.setXlinkTypeAttribute("simple");
		// A draw:image is always embedded in a draw:frame
		InputStream is = mOdfSchemaDoc.getPackage().getInputStream(packagePath);
		DrawFrameElement odfFrame = (DrawFrameElement) imageElement.getParentNode();
		Frame aFrame = getInstanceofFrame(odfFrame);
		FrameRectangle oldRect = aFrame.getRectangle();
		if (oldRect.getLinearMeasure() != StyleTypeDefinitions.SupportedLinearMeasure.CM)
			oldRect.setLinearMeasure(StyleTypeDefinitions.SupportedLinearMeasure.CM);
		if (odfFrame != null) {
			BufferedImage image = ImageIO.read(is);
			int height = image.getHeight(null);
			int width = image.getWidth(null);
			odfFrame.setSvgHeightAttribute(Length.mapToUnit(String.valueOf(height) + "px", Unit.CENTIMETER));
			odfFrame.setSvgWidthAttribute(Length.mapToUnit(String.valueOf(width) + "px", Unit.CENTIMETER));
			if (isResetSize) {
				FrameRectangle newRect = aFrame.getRectangle();
				newRect.setX(oldRect.getX()+(oldRect.getWidth()-newRect.getWidth())/2);
				newRect.setY(oldRect.getY()+(oldRect.getHeight()-newRect.getHeight())/2);
				aFrame.setRectangle(newRect);
			}
		}
		return uri;
	}

	protected static Frame getInstanceofFrame(DrawFrameElement element) {
		Frame frame = null;
		try {
		  Class fr = Frame.class;
		  Method getInstance = fr.getDeclaredMethod("getInstanceof", DrawFrameElement.class);
		  getInstance.setAccessible(true);
		  frame = (Frame) getInstance.invoke(null, new Object[]{element});
		} catch(Exception e) {
			e.printStackTrace();
		}
		return frame;
	}

	/*
	 * Delete the <code>pNode<code> from the <code>fromIndex</code> text, and
	 * delete <code>leftLength</code> text.
	 */
	private void delete(int fromIndex, int leftLength, Node pNode) {
		if ((fromIndex == 0) && (leftLength == 0)) {
			return;
		}
		int nodeLength = 0;
		Node node = pNode.getFirstChild();
		while (node != null) {
			if ((fromIndex == 0) && (leftLength == 0)) {
				return;
			}
			if (node.getNodeType() == Node.TEXT_NODE) {
				nodeLength = node.getNodeValue().length();
			} else if (node.getNodeType() == Node.ELEMENT_NODE) {
				// text:s
				if (node.getLocalName().equals("s")) {
					try {
						nodeLength = Integer.parseInt(((Element) node).getAttributeNS(OdfDocumentNamespace.TEXT
								.getUri(), "c"));
					} catch (Exception e) {
						nodeLength = 1;
					}
				} else if (node.getLocalName().equals("line-break")) {
					nodeLength = 1;
				} else if (node.getLocalName().equals("tab")) {
					nodeLength = 1;
				} else {
					nodeLength = TextExtractor.getText((OdfElement) node).length();
				}
			}
			if (nodeLength <= fromIndex) {
				fromIndex -= nodeLength;
			} else {
				// the start index is in this node
				if (node.getNodeType() == Node.TEXT_NODE) {
					String value = node.getNodeValue();
					StringBuffer buffer = new StringBuffer();
					buffer.append(value.substring(0, fromIndex));
					int endLength = fromIndex + leftLength;
					int nextLength = value.length() - endLength;
					fromIndex = 0;
					if (nextLength >= 0) {
						// delete the result
						buffer.append(value.substring(endLength, value.length()));
						leftLength = 0;
					} else {
						leftLength = endLength - value.length();
					}
					node.setNodeValue(buffer.toString());

				} else if (node.getNodeType() == Node.ELEMENT_NODE) {
					// if text:s?????????
					// text:s
					if (node.getLocalName().equals("s")) {
						// delete space
						((TextSElement) node).setTextCAttribute(new Integer(nodeLength - fromIndex));
						leftLength = leftLength - (nodeLength - fromIndex);
						fromIndex = 0;
					} else if (node.getLocalName().equals("line-break") || node.getLocalName().equals("tab")) {
						fromIndex = 0;
						leftLength--;
					} else {
						delete(fromIndex, leftLength, node);
						int length = (fromIndex + leftLength) - nodeLength;
						leftLength = length > 0 ? length : 0;
						fromIndex = 0;
					}
				}
			}
			node = node.getNextSibling();
		}
	}

	/**
	 * Construct a ImageSelection with TextSelection. Then user can replace text
	 * content with Image from Document or Image form System
	 *
	 * @param selection
	 *            the TextSelection to be decorated.
	 */
	public ImageSelection(TextSelection selection) {
		textSelection = selection;
		imageContainer = null;
	}

	/**
	 * Delete the selection from the document the other matched selection in the
	 * same container element will be updated automatically because the start
	 * index of the following selections will be changed when the previous
	 * selection has been deleted.
	 *
	 * @throws InvalidNavigationException
	 *             if the selection is unavailable.
	 */
	@Override
	public void cut() throws InvalidNavigationException {
		textSelection.cut();
	}

	/**
	 * Paste this selection just after a specific selection.
	 *
	 * @param positionItem
	 *            a selection that is used to point out the position
	 * @throws InvalidNavigationException
	 *             if the selection is unavailable.
	 */
	@Override
	public void pasteAtEndOf(Selection positionItem) throws InvalidNavigationException {
		textSelection.pasteAtEndOf(positionItem);
	}


	/**
	 * Paste this selection just before a specific selection.
	 *
	 * @param positionItem
	 *            a selection that is used to point out the position
	 * @throws InvalidNavigationException
	 *             if the selection is unavailable.
	 */
	@Override
	public void pasteAtFrontOf(Selection positionItem) throws InvalidNavigationException {
		textSelection.pasteAtFrontOf(positionItem);
	}

	protected void refresh(int offset) {
		textSelection.refresh(offset);
	}

	protected void refreshAfterFrontalDelete(Selection deletedItem) {
		textSelection.refreshAfterFrontalDelete(deletedItem);
	}

	protected void refreshAfterFrontalInsert(Selection insertedItem) {
		textSelection.refreshAfterFrontalInsert(insertedItem);
	}

	private void insertOdfElement(OdfElement odfElement, int fromIndex, Node pNode) {
		if (fromIndex < 0) {
			fromIndex = 0;
		}
		if (fromIndex == 0 && mIsInserted) {
			return;
		}
		int nodeLength = 0;
		Node node = pNode.getFirstChild();
		while (node != null) {
			if (fromIndex <= 0 && mIsInserted) {
				return;
			}
			if (node.getNodeType() == Node.TEXT_NODE) {
				nodeLength = node.getNodeValue().length();
				if ((fromIndex != 0) && (nodeLength < fromIndex)) {
					fromIndex -= nodeLength;
				} else {
					// insert result after node, and insert an new text node
					// after the result node
					String value = node.getNodeValue();
					StringBuffer buffer = new StringBuffer();
					buffer.append(value.substring(0, fromIndex));
					// insert the text span in appropriate position
					node.setNodeValue(buffer.toString());
					Node nextNode = node.getNextSibling();
					Node parNode = node.getParentNode();
					Node newNode = node.cloneNode(true);
					newNode.setNodeValue(value.substring(fromIndex, value.length()));
					if (nextNode != null) {
						parNode.insertBefore(odfElement, nextNode);
						parNode.insertBefore(newNode, nextNode);
					} else {
						parNode.appendChild(odfElement);
						parNode.appendChild(newNode);
					}
					mIsInserted = true;
					return;
				}
			} else if (node.getNodeType() == Node.ELEMENT_NODE) {
				// text:s
				if (node.getLocalName().equals("s")) {
					try {
						nodeLength = Integer.parseInt(((Element) node).getAttributeNS(OdfDocumentNamespace.TEXT
								.getUri(), "c"));
					} catch (Exception e) {
						nodeLength = 1;
					}
					fromIndex -= nodeLength;
				} else if (node.getLocalName().equals("line-break")) {
					nodeLength = 1;
					fromIndex--;
				} else if (node.getLocalName().equals("tab")) {
					nodeLength = 1;
					fromIndex--;
				} else {
					nodeLength = TextExtractor.getText((OdfElement) node).length();
					insertOdfElement(odfElement, fromIndex, node);
					fromIndex -= nodeLength;
				}
			}
			node = node.getNextSibling();
		}
	}

}
