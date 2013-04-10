/*
 * University of British Columbia
 * Department of Computer Science
 * CPSC317 - Internet Programming
 * Assignment 1
 * 
 * Author: Yelnil Gabo g3d6 70179064
 * January 2013
 * 
 * This code may not be used without written consent of the authors, except for 
 * current and future projects and assignments of the CPSC317 course at UBC.
 */

package ubc.cs317.xmpp.net;

import java.io.IOException;
import java.net.Socket;
import java.net.UnknownHostException;
import java.util.Date;

import javax.xml.bind.DatatypeConverter;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import ubc.cs317.xmpp.exception.XMPPException;
import ubc.cs317.xmpp.model.Contact;
import ubc.cs317.xmpp.model.ContactStatus;
import ubc.cs317.xmpp.model.Conversation;
import ubc.cs317.xmpp.model.Message;
import ubc.cs317.xmpp.model.Session;

/**
 * This class describes the XMPP connection handler. A socket connection is
 * created when an instance of this handler is created, and methods are provided
 * for most common operations in XMPP.
 * 
 * This class will not in any case make a direct reference to any class or
 * method that represents a specific UI library.
 */
public class XMPPConnection {

	/**
	 * Default TCP port for client-server communication in XMPP.
	 */
	public static final int XMPP_DEFAULT_PORT = 5222;

	/**
	 * Session object for communication between the network component and the
	 * chat model and UI.
	 */
	private Session session;

	/**
	 * Socket object associated to the communication between this client and the
	 * XMPP server.
	 */
	private Socket socket;

	/**
	 * XMPP reader helper, used to obtain XML nodes from the XMPP stream.
	 */
	private XMPPStreamReader xmppReader;

	/**
	 * XMPP writer helper, used to write XML nodes to the XMPP stream.
	 */
	private XMPPStreamWriter xmppWriter;

	private Element features;

	public static String CLIENT_DEFAULT_MECH = "PLAIN";

	private boolean debug = true;

	private Integer myID = 0;

	/**
	 * Creates a new instance of the connection handler. This constructor will
	 * creating the socket, initialise the reader and writer helpers, send
	 * initial tags, authenticate the user and bind to a resource.
	 * 
	 * @param jidUser
	 *            User part of the Jabber ID.
	 * @param jidDomain
	 *            Domain part of the Jabber ID.
	 * @param resource
	 *            Resource to bind once authenticated. If null or empty, a new
	 *            resource will be generated.
	 * @param password
	 *            Password for authentication.
	 * @param session
	 *            Instance of the session to communicate with other parts of the
	 *            system.
	 * @throws XMPPException
	 *             If there is an error establishing the connection, sending or
	 *             receiving necessary data, or while authenticating.
	 */
	public XMPPConnection(String jidUser, String jidDomain, String resource,
			String password, Session session) throws XMPPException {

		this.session = session;

		initializeConnection(jidDomain);

		try {
			xmppReader = new XMPPStreamReader(socket.getInputStream());
			xmppWriter = new XMPPStreamWriter(socket.getOutputStream());
		} catch (XMPPException e) {
			throw e;
		} catch (Exception e) {
			throw new XMPPException("Could not obtain socket I/O channels ("
					+ e.getMessage() + ")", e);
		}

		initializeStreamAndFeatures(jidUser, jidDomain);

		login(jidUser, password);

		bindResource(resource);

		startListeningThread();
	}

	/**
	 * Initialises the connection with the specified domain. This method sets
	 * the socket field with an initialised socket.
	 * 
	 * @param domain
	 *            DNS name (or IP string) of the server to connect to.
	 * @throws XMPPException
	 *             If there is a problem connecting to the server.
	 */
	private void initializeConnection(String domain) throws XMPPException {

		try {
			this.socket = new Socket(domain, XMPP_DEFAULT_PORT);
		} catch (UnknownHostException e) {
			throw new XMPPException(e);
		} catch (IOException e) {
			throw new XMPPException(e);
		}
	}

	/**
	 * Sends the initial data to establish an XMPP connection stream with the
	 * XMPP server. This method also retrieves the set of features from the
	 * server, saving it in a field for future use.
	 * 
	 * @param jidUser
	 *            User part of the Jabber ID.
	 * @param jidDomain
	 *            Domain part of the Jabber ID.
	 * @throws XMPPException
	 *             If there is a problem sending or receiving the data.
	 */
	private void initializeStreamAndFeatures(String jidUser, String jidDomain)
			throws XMPPException {

		try {
			// create the initial xml, looks like this:
			// <stream:stream from='bunnylol' to='jabber.org' version='1.0'
			// xml:lang='en' xmlns='jabber:client'
			// xmlns:stream='http://etherx.jabber.org/streams'>;
			Element root = xmppWriter.createRootElement("stream:stream");
			root.setAttribute("from", jidUser);
			root.setAttribute("to", jidDomain);
			root.setAttribute("version", "1.0");
			root.setAttribute("xml:lang", "en");
			root.setAttribute("xmlns", "jabber:client");
			root.setAttribute("xmlns:stream",
					"http://etherx.jabber.org/streams");

			// send it
			xmppWriter.writeIndividualElement(root);

			// save the features that the server sent back
			this.features = xmppReader.readSecondLevelElement();

			if (this.debug) {
				System.out.println();
				xmppWriter.debugElement(System.out, this.features);
			}

		} catch (Exception e) {
			throw new XMPPException(e);
		}
	}

	/**
	 * Attempts to authenticate the user name with the provided password at the
	 * connected server. This method will verify if the server supports the
	 * implemented authentication mechanism(s) and send the user and password
	 * based on the first mechanism it finds. In case authentication is not
	 * successful, this function will close the connection and throw an
	 * XMPPException. This function also retrieves the new set of features
	 * available after authentication.
	 * 
	 * @param username
	 *            User name to use for authentication.
	 * @param password
	 *            Password to use for authentication.
	 * @throws XMPPException
	 *             If authentication is not successful, or if authentication
	 *             methods supported by the server are not implemented, or if
	 *             there was a problem sending authentication data.
	 */
	private void login(String username, String password) throws XMPPException {

		// check if the server supports PLAIN
		NodeList mechList = this.features.getElementsByTagName("mechanisms");
		NodeList mechs = mechList.item(0).getChildNodes();
		int numMechs = mechs.getLength();
		boolean mechFound = false;

		// check the list of mechs if PLAIN is in there
		for (int i = 0; i < numMechs; i++) {
			if (mechs.item(i).getTextContent().equals(CLIENT_DEFAULT_MECH)) {
				mechFound = true;
			}

			if (this.debug) {
				System.out.println(mechs.item(i).getTextContent());
			}
		}

		// couldn't find PLAIN
		if (!mechFound) {
			closeConnection();
			throw new XMPPException(
					"Error: PLAIN mechanism not supported by server.");
		}

		// server supports PLAIN, create the login xml to send
		Element login = xmppWriter.createElement("auth");
		login.setAttribute("xmlns", "urn:ietf:params:xml:ns:xmpp-sasl");
		login.setAttribute("mechanism", "PLAIN");

		// this is the PLAIN setup
		String hey = "\0" + username + "\0" + password;
		String hey64 = DatatypeConverter.printBase64Binary(hey.getBytes());

		login.setTextContent(hey64);

		// send the xml
		xmppWriter.writeIndividualElement(login);

		Element response = xmppReader.readSecondLevelElement();
		// see if the response has a 'fail' word in it
		if (response.getTagName().toLowerCase().contains("fail")) {
			closeConnection();
			throw new XMPPException("Log in failed.");
		}// if not, we proceed

		// we need to restart stream, so re-send the <stream> element here
		xmppWriter.writeRootElementWithoutClosingTag();

		// save the features again, since we restarted
		this.features = xmppReader.readSecondLevelElement();

		if (this.debug) {
			System.out.println();
			xmppWriter.debugElement(System.out, this.features);
		}
	}

	/**
	 * Binds the connection to a specific resource, or retrieves a
	 * server-generated resource if one is not provided. This function will wait
	 * until a resource is sent by the server.
	 * 
	 * @param resource
	 *            Name of the user-specified resource. If resource is null or
	 *            empty, retrieves a server-generated one.
	 * @throws XMPPException
	 *             If there is an error sending or receiving the data.
	 */
	private void bindResource(String resource) throws XMPPException {

		// see if resource is given by user
		boolean userProvided = (resource != null) && (resource.length() != 0);

		// create the xml about resource
		Element bindIQ = xmppWriter.createElement("iq");
		bindIQ.setAttribute("id", getIDHelper());
		bindIQ.setAttribute("type", "set");

		Element bindB = xmppWriter.createElement("bind");
		bindB.setAttribute("xmlns", "urn:ietf:params:xml:ns:xmpp-bind");

		// if user provided a resource, use it
		if (userProvided) {
			Element resourceData = xmppWriter.createElement("resource");
			resourceData.setTextContent(resource);
			bindB.appendChild(resourceData);
		}

		bindIQ.appendChild(bindB);

		// now send the resource bind request
		xmppWriter.writeIndividualElement(bindIQ);

		// get the response
		Element serverResponse = xmppReader.readSecondLevelElement();

		// get the jid that the server gave us
		NodeList newJidList = serverResponse.getElementsByTagName("jid");
		Element userJid = (Element) newJidList.item(0);

		// if server returned error
		if (serverResponse.getAttribute("type").toLowerCase().equals("error")
				|| newJidList.getLength() < 1) {
			closeConnection();
			throw new XMPPException("Error: Resource binding failed.");
		}

		// that will be what the new jid
		this.session.setUserJid(userJid.getTextContent());

		// debug
		if (this.debug) {
			System.out.print("JID: ");
			xmppWriter.debugElement(System.out, userJid);
			System.out.println();
			xmppWriter.debugElement(System.out, bindIQ);
			System.out.println();
			xmppWriter.debugElement(System.out, serverResponse);
			System.out.println();
			System.out.println(" COMPLETE? : "
					+ xmppReader.isDocumentComplete());
		}
	}

	/**
	 * Starts a thread that will keep listening for new messages asynchronously
	 * from the main thread.
	 */
	private void startListeningThread() {
		Thread listeningThread = new Thread(new Runnable() {

			@Override
			public void run() {
				listeningProcess();
			}
		});
		listeningThread.start();
	}

	/**
	 * Keeps listening for new XML elements in a loop until a closing tag is
	 * found or an exception happens. If an exception happens, calls
	 * <code>session.processReceivedException</code> and closes the connection.
	 * If the closing tag is found, sends a closing tag back and closes the
	 * connection as well. In both cases, the connection is closed at the
	 * session level, so that the model and UI can handle the connection being
	 * closed. For each received element, processes the element and handles it
	 * accordingly.
	 */
	private void listeningProcess() {

		// initial variables
		Contact currentContact;
		Element contact, input, inputChild;
		String contactJid, alias, sub;

		// main loop
		while (true) {
			try {
				// This is the main reader
				// blocks here if nothing's coming in
				input = xmppReader.readSecondLevelElement();
				inputChild = (Element) input.getFirstChild();

				// server sent a closing tag ( document is complete )
				// or if socket is closed, break the loop
				if (xmppReader.isDocumentComplete() || this.socket.isClosed()) {
					// close connection handles if socket is closed ( for some
					// reason)
					closeConnection();
					break;
				}

				// For IQ results
				if (input.getAttribute("type").toLowerCase().equals("result")
						&& input.getTagName().toLowerCase().equals("iq")) {

					// if initial roster query
					if (inputChild != null) {
						if (inputChild.getTagName().toLowerCase()
								.equals("query")
								&& inputChild.getAttribute("xmlns")
										.toLowerCase()
										.equals("jabber:iq:roster")) {

							// retrieve the contacts from the XML
							NodeList contactList = input
									.getElementsByTagName("item");
							int cLength = contactList.getLength();

							// loop through all contacts
							for (int i = 0; i < cLength; i++) {

								// current contact
								contact = (Element) contactList.item(i);
								contactJid = contact.getAttribute("jid");
								alias = contact.getAttribute("name");
								sub = contact.getAttribute("subscription");

								currentContact = new Contact(contactJid, alias);

								// if not yet added in local list, add contact
								if (this.session.getContact(contactJid) == null
										&& !sub.equals("none")) {
									this.session
											.addReceivedContact(currentContact);
								}
							}
						}
					}
				}
				// when we get a new contact to add/update
				else if (input.getAttribute("type").toLowerCase().equals("set")
						&& input.getTagName().toLowerCase().equals("iq")) {

					// retrieve the contact from the XML
					NodeList contactList = input.getElementsByTagName("item");

					// current contact to add to our list
					contact = (Element) contactList.item(0);
					contactJid = contact.getAttribute("jid");
					// see if there's an alias, if not, null
					alias = contact.getAttribute("name").length() > 0 ? contact
							.getAttribute("name") : null;
					currentContact = new Contact(contactJid, alias);

					if (contact.getAttribute("subscription").equals("remove"))
						this.session.removeContact(this.session
								.getContact(contactJid));
					else {
						// add contact or remove contact
						if (this.session.getContact(contactJid) == null)
							this.session.addReceivedContact(currentContact);
					}
				}
				// if input was about a <presence> stanza
				else if (input.getTagName().toLowerCase().equals("presence")) {

					String jid = input.getAttribute("from");
					String[] splitFrom = jid.split("/");
					String show = null;
					String status = null;
					ContactStatus newStatus = null;

					// get the bare jid
					String from = splitFrom[0];

					// get resource, null if there isn't any
					String resource = (splitFrom.length > 1) ? splitFrom[1]
							: null;

					NodeList showList = input.getElementsByTagName("show");

					NodeList statusList = input.getElementsByTagName("status");

					if (statusList.getLength() > 0)
						status = statusList.item(0).getTextContent();

					// if there's a <show> tag
					if (showList.getLength() > 0) {
						// away -- The entity or resource is temporarily away.
						// chat -- The entity or resource is actively interested
						// in chatting.
						// dnd -- The entity or resource is busy.
						// xa -- The entity or resource is away for an extended
						// period.

						show = showList.item(0).getTextContent();
						if (show.equals("away")) {
							newStatus = ContactStatus.AWAY;
						} else if (show.equals("chat")) {
							newStatus = ContactStatus.CHAT;
						} else if (show.equals("dnd")) {
							newStatus = ContactStatus.DND;
						} else if (show.equals("xa")) {
							newStatus = ContactStatus.XA;
						}

						// get the contact that has a new presence/status
						Contact contactToChange = this.session.getContact(from);

						// if we found the contact in our list
						if (contactToChange != null)
							contactToChange.setStatus(resource, newStatus);
					}
					// no <show> tag, no type attribute as well
					// this means the contact is available
					else if ((showList.getLength() == 0)
							&& (input.getAttribute("type").toLowerCase()
									.equals(""))) {
						newStatus = ContactStatus.AVAILABLE;

						// get the contact that has a new presence/status
						Contact contactToChange = this.session.getContact(from);

						// if we found the contact in our list
						if (contactToChange != null)
							contactToChange.setStatus(resource, newStatus);
					}
					// no <show> tag, but the type attribute
					// says 'unavailable' -- meaning contact has
					// gone offline
					else if ((showList.getLength() == 0)
							&& (input.getAttribute("type").toLowerCase()
									.equals("unavailable"))) {
						newStatus = ContactStatus.OFFLINE;

						// get the contact that has a new presence/status
						Contact contactToChange = this.session.getContact(from);

						// if we found the contact in our list
						if (contactToChange != null)
							contactToChange.setStatus(resource, newStatus);
					}
					// no <show> tag, but the type attribute
					// is subscribe... means someone wants to add us
					else if ((showList.getLength() == 0)
							&& (input.getAttribute("type").toLowerCase()
									.equals("subscribe"))) {
						this.session.handleReceivedSubscriptionRequest(from);
					}
					// no <show> tag, but the type attribute
					// says subscribed, means someone approved
					else if ((showList.getLength() == 0)
							&& (input.getAttribute("type").toLowerCase()
									.equals("subscribed"))) {
						sendCurrentStatus();
					}
					// someone unsubscribed
					else if ((showList.getLength() == 0)
							&& (input.getAttribute("type")
									.equals("unsubscribed"))) {

						if (this.session.getContact(from) != null)
							this.session.removeContact(this.session
									.getContact(from));
					}
				}
				// if we get a <message> stanza with type='chat'
				else if (input.getTagName().toLowerCase().equals("message")
						&& input.getAttribute("type").toLowerCase()
								.equals("chat")) {

					// we know its a chat message, get the details
					String from = input.getAttribute("from");
					NodeList body = input.getElementsByTagName("body");

					// only add the message to the convo
					// if there was a message
					if (body.getLength() > 0) {
						String bodyMessage = input.getElementsByTagName("body")
								.item(0).getTextContent();

						// search our contact list for the contact that sent the
						// message
						Contact fromC = this.session.getContact(from);

						// recreate the info into a message class
						// 'to' is null since its for the us
						// new timestamp so create a new Date class
						Message message = new Message(fromC, null, bodyMessage,
								new Date());

						// search the conversation with the contact
						Conversation currentConvo = this.session
								.getConversation(fromC);

						// add the message to the conversation
						currentConvo.addIncomingMessage(message,
								from.split("/")[1]);
					}
				}

				if (this.debug) {
					System.out.println();
					xmppWriter.debugElement(System.out, input);
				}

			} catch (Exception e) {
				this.session.processReceivedException(new XMPPException(e));
				closeConnection();
				break;
			}
		}
	}

	/**
	 * Closes the connection. If the connection was already closed before this
	 * method is called nothing is done, otherwise sends all necessary closing
	 * data and waits for the server to send the closing data as well. Once this
	 * happens the socket connection is closed. This method does not throw any
	 * exception, choosing instead to ignore them. However, even if an exception
	 * happens while sending the final data, the socket connection will be
	 * closed.
	 */
	public synchronized void closeConnection() {

		try {
			// send the closing tag
			if (!this.socket.isClosed() && !xmppReader.isDocumentComplete()) {
				xmppWriter.writeCloseTagRootElement();
			}

		} catch (XMPPException e) {
			e.printStackTrace();
		} finally {
			try {
				// close socket if it isn't closed
				if (!this.socket.isClosed())
					this.socket.close();
			} catch (IOException e) {
				e.printStackTrace();
			}
		}

	}

	/**
	 * Sends a request for the contact list. The result is not expected to be
	 * received in this function, but it should come in a message that will be
	 * handled by the listening process.
	 * 
	 * @throws XMPPException
	 *             If there was a problem sending the request.
	 */
	public void sendRequestForContactList() throws XMPPException {

		// XML looks like this:
		// <iq from='juliet@example.com/balcony' id='bv1bs71f' type='get'><query
		// xmlns='jabber:iq:roster'/></iq>

		// create the above xml
		Element contacts = xmppWriter.createElement("iq");
		contacts.setAttribute("from", this.session.getUserJid());
		contacts.setAttribute("id", getIDHelper());
		contacts.setAttribute("type", "get");

		Element query = xmppWriter.createElement("query");
		query.setAttribute("xmlns", "jabber:iq:roster");

		contacts.appendChild(query);

		// now send the xml request to the server
		xmppWriter.writeIndividualElement(contacts);
	}

	/**
	 * Sends an updated status information to the server, based on the status
	 * currently attributed to the session.
	 * 
	 * @throws XMPPException
	 *             If there was a problem sending the status.
	 */
	public void sendCurrentStatus() throws XMPPException {
		sendStatus(session.getCurrentStatus());
	}

	/**
	 * Sends a specific status information to the server.
	 * 
	 * @param status
	 *            Status to send to the server.
	 * @throws XMPPException
	 *             If there was a problem sending the status.
	 */
	private void sendStatus(ContactStatus status) throws XMPPException {

		// create presence xml
		Element presence = xmppWriter.createElement("presence");

		if (status.getXmppShow() != null) {
			Element show = xmppWriter.createElement("show");
			show.setTextContent(status.getXmppShow());
			presence.appendChild(show);
		}

		// send the presence xml to the server
		xmppWriter.writeIndividualElement(presence);
	}

	/**
	 * Sends a request that a new contact be added to the list of contacts.
	 * Additionally, requests authorization from that contact to receive updates
	 * any time the contact changes its status. This function does not add the
	 * user to the local list of contacts, which happens at the listening
	 * process once the server sends an update to the list of contacts as a
	 * result of this request.
	 * 
	 * @param contact
	 *            Contact that should be requested.
	 * @throws XMPPException
	 *             If there is a problem sending the request.
	 */
	public void sendNewContactRequest(Contact contact) throws XMPPException {

		// XML for this looks like:
		// <iq from='juliet@example.com/balcony' id='ph1xaz53' type='set'>
		// <query xmlns='jabber:iq:roster'>
		// <item jid='nurse@example.com' name='Nurse'>
		// <group>Servants</group>
		// </item></query></iq>

		// main iq tag
		Element IQ = xmppWriter.createElement("iq");
		IQ.setAttribute("from", this.session.getUserJid());
		IQ.setAttribute("id", getIDHelper());
		IQ.setAttribute("type", "set");

		// query tag
		Element query = xmppWriter.createElement("query");
		query.setAttribute("xmlns", "jabber:iq:roster");

		// item tag
		Element item = xmppWriter.createElement("item");
		// bare JID means no resource (just contact@domain)
		item.setAttribute("jid", contact.getBareJid());
		item.setAttribute("name", contact.getAlias());

		// append them together
		query.appendChild(item);
		IQ.appendChild(query);

		// send the xml request to add new contact
		xmppWriter.writeIndividualElement(IQ);

		// now we have to create a presence xml:
		// <presence id='xk3h1v69' to='juliet@example.com' type='subscribe'/>
		// this asks for a subscription to the contact we just added
		// we are basically 'subscribing' to this contact's status changes
		Element presence = xmppWriter.createElement("presence");
		presence.setAttribute("id", getIDHelper());
		presence.setAttribute("to", contact.getBareJid());
		presence.setAttribute("type", "subscribe");

		// now send the presence subscription request
		xmppWriter.writeIndividualElement(presence);

		// we don't have to handle anything
		// 'listen' thread will do that

		if (this.debug) {
			System.out.println();
			System.out.print("outgoing contact add: ");
			xmppWriter.debugElement(System.out, IQ);
			System.out.println();
			System.out.print("outgoing presence: ");
			xmppWriter.debugElement(System.out, presence);
		}
	}

	/**
	 * Sends a response message to a contact that requested authorization to
	 * receive updates when the local user changes its status.
	 * 
	 * @param jid
	 *            Jabber ID of the contact that requested authorization.
	 * @param accepted
	 *            <code>true</code> if the request was accepted by the user,
	 *            <code>false</code> otherwise.
	 * @throws XMPPException
	 *             If there was an error sending the response.
	 */
	public void respondContactRequest(String jid, boolean accepted)
			throws XMPPException {

		// this XML is sent to the server to notify
		// if we accepted or declined a contact request
		// looks like this:
		// <presence id='h4v1c4kj' to='romeo@example.net' type='subscribed'/>

		// create XML
		Element presence = xmppWriter.createElement("presence");
		presence.setAttribute("id", getIDHelper());
		presence.setAttribute("to", jid);
		if (accepted)
			presence.setAttribute("type", "subscribed");
		else
			presence.setAttribute("type", "unsubscribed");

		// now send it
		xmppWriter.writeIndividualElement(presence);
	}

	/**
	 * Request that the server remove a specific contact from the list of
	 * contacts. Additionally, requests that no further status updates be sent
	 * regarding that contact, as well as that no further status updates about
	 * the local user be sent to that contact. This function does not remove the
	 * user from the local list of contacts, which happens at the listening
	 * process once the server sends an update to the list of contacts as a
	 * result of this request.
	 * 
	 * @param contact
	 *            Contact to be removed from the list of contacts.
	 * @throws XMPPException
	 *             If there was an error sending the request.
	 */
	public void removeAndUnsubscribeContact(Contact contact)
			throws XMPPException {

		// to remove a contact, just send this XML to server
		// <iq from='juliet@example.com/balcony' id='hm4hs97y' type='set'>
		// <query xmlns='jabber:iq:roster'>
		// <item jid='nurse@example.com' subscription='remove'/>
		// </query> </iq>

		// main iq tag
		Element IQ = xmppWriter.createElement("iq");
		IQ.setAttribute("from", this.session.getUserJid());
		IQ.setAttribute("id", getIDHelper());
		IQ.setAttribute("type", "set");

		// query tag
		Element query = xmppWriter.createElement("query");
		query.setAttribute("xmlns", "jabber:iq:roster");

		// item tag
		Element item = xmppWriter.createElement("item");
		// bare JID means no resource (just contact@domain)
		item.setAttribute("jid", contact.getBareJid());
		item.setAttribute("subscription", "remove");

		// append them together
		query.appendChild(item);
		IQ.appendChild(query);

		// send the xml request to add new contact
		xmppWriter.writeIndividualElement(IQ);

		// this method doesn't actually remove,
		// just sends a request and the listen thread
		// listens for the trigger to remove
		
		Element presence = xmppWriter.createElement("presence");
		presence.setAttribute("id", getIDHelper());
		presence.setAttribute("to", contact.getBareJid());
		presence.setAttribute("type", "unsubscribed");

		// now send the presence subscription request
		xmppWriter.writeIndividualElement(presence);
		
		presence = xmppWriter.createElement("presence");
		presence.setAttribute("id", getIDHelper());
		presence.setAttribute("to", contact.getBareJid());
		presence.setAttribute("type", "unsubscribe");

		// now send the presence subscription request
		xmppWriter.writeIndividualElement(presence);

		if (this.debug) {
			System.out.println();
			System.out.print("outgoing - remove contact: ");
			xmppWriter.debugElement(System.out, IQ);
		}
		
		
		
	}

	/**
	 * Send a chat message to a specific contact.
	 * 
	 * @param message
	 *            Message to be sent.
	 * @throws XMPPException
	 *             If there was a problem sending the message.
	 */
	public void sendMessage(Message message) throws XMPPException {

		// create the message xml
		Element messageXML = xmppWriter.createElement("message");
		messageXML.setAttribute("from", session.getUserJid());
		messageXML.setAttribute("id", getIDHelper());
		messageXML.setAttribute("to", message.getTo().getBareJid());
		messageXML.setAttribute("type", "chat");
		messageXML.setAttribute("xml:lang", "en");

		Element body = xmppWriter.createElement("body");
		body.setTextContent(message.getTextMessage());

		messageXML.appendChild(body);

		// send the message xml
		xmppWriter.writeIndividualElement(messageXML);
	}

	// private helper to get unique IDs
	private String getIDHelper() {

		// increment the ID
		this.myID++;

		// send it
		return this.myID.toString();
	}
}
