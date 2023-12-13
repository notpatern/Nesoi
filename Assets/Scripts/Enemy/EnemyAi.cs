using UnityEngine;
using UnityEngine.AI;
using UnityEngine.Serialization;
using Random = UnityEngine.Random;

public class EnemyAi : MonoBehaviour , IDamageable
{
   [SerializeField] public int health;
   [SerializeField] public int damage;
   
   [SerializeField] private NavMeshAgent agent;
   public AudioSource attackAudio;
   public AudioSource deathAudio;
   [SerializeField] private EnemyProjectile enemyProjectile;
   [SerializeField] private Transform player;
   [SerializeField] private Transform aimingDevice;
   [SerializeField] private GameObject blood;
   [SerializeField] private LayerMask isGround;
   [SerializeField] public LayerMask isPlayer;

   [SerializeField] Vector3 walkPoint;
   private bool walkPointSet;
   [SerializeField] private float walkPointRange;

   [SerializeField] private float timeBetweenAttacks, timeBetweenAttacksInaccuracy;
   private bool alreadyAttacked;

   [SerializeField] public float sightRange;
   [SerializeField] public float attackRange;
   [SerializeField] private float sightInaccuracy, attackInaccuracy;
   [SerializeField] public bool playerInSightRange;
   [SerializeField] public bool playerInAttackRange;

   private void Awake()
   {
      player = GameObject.Find("PlayerModel").transform;
      agent = GetComponent<NavMeshAgent>();
      enemyProjectile = GetComponent<EnemyProjectile>();
      sightRange += Random.Range(-sightInaccuracy, sightInaccuracy);
      attackRange += Random.Range(-attackInaccuracy, attackInaccuracy);
   }

   public virtual void Update()
   {
      var position = transform.position;
      playerInSightRange = Physics.CheckSphere(position, sightRange, isPlayer);
      playerInAttackRange = Physics.CheckSphere(position, attackRange, isPlayer);
      
      if (!playerInSightRange && !playerInAttackRange) Patrolling();
      if (playerInSightRange && !playerInAttackRange) ChasePlayer();
      if (playerInSightRange && playerInAttackRange) AttackPlayer();
   }

   public void Patrolling()
   {
      if (!walkPointSet) SearchWalkPoint();
      else agent.SetDestination(walkPoint);

      Vector3 distanceToWalkPoint = transform.position - walkPoint;
      if (distanceToWalkPoint.magnitude < 1f) walkPointSet = false;
   }

   private void SearchWalkPoint()
   {
      float randomZ = Random.Range(-walkPointRange, walkPointRange);
      float randomX = Random.Range(-walkPointRange, walkPointRange);

      walkPoint = new Vector3(transform.position.x + randomX, transform.position.y, transform.position.z + randomZ);
      if (Physics.Raycast(walkPoint, -transform.up, 2f, isGround)) walkPointSet = true;
   }


   public void ChasePlayer()
   {
         agent.SetDestination(player.position);
   }

   public void AttackPlayer()
   {
      agent.SetDestination(transform.position);
      
      agent.transform.LookAt(new Vector3(player.position.x, agent.transform.position.y, player.position.z));
      aimingDevice.LookAt(new Vector3(player.position.x, player.position.y + 1f, player.position.z));

      if (alreadyAttacked) return;
      enemyProjectile.ThrowProjectile(aimingDevice);
      alreadyAttacked = true;
      Invoke(nameof(ResetAttack), timeBetweenAttacks);
   }

   private void ResetAttack()
   {
      timeBetweenAttacks = Random.Range(3, timeBetweenAttacksInaccuracy);
      alreadyAttacked = false;
   }

   public virtual void OnTriggerEnter(Collider other)
   {
      if (!other.CompareTag("Player")) return;
      var target = other.GetComponent<IDamageable>();
      if (target == null) return;
      if (!target.CanDamage()) return;
      TakeDamage(target.Damage());
   }

   public bool CanDamage()
   {
      return true;
   }

   public int Damage()
   {
      return damage;
   }

   public void TakeDamage(int damage)
   {
      Destroy(Instantiate(blood, transform.position, Quaternion.LookRotation(-player.forward)), 1f);
      health -= damage;
      if (health > 0) return;
      Destroy(gameObject);
   }
}
